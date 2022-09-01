-- The example uses https://www.jooq.org/sakila, but you can just replace
-- the "source" table with anything else
with 

  -- This part is what you can modify to adapt to your own needs
  --------------------------------------------------------------

  -- Your data producing query here 
  source (key, value) as (
    select payment_date::date::timestamp, sum(amount)
    from payment
    where extract(year from payment_date) < 2006
    group by payment_date::date::timestamp
    order by payment_date::date::timestamp
  ),
  
  -- Some configuration items:
  constants as (
    select
    
      -- the height of the y axis
      15 as height, 

      -- the width of the x axis, if normalise_x, otherwise, ignored
      25 as width, 

      -- the bar characters
      '##' as characters,

      -- the characters between bars
      ' ' as separator,
      
      -- the padding of the labels on the y axis
      10 as label_pad, 
      
      -- whether to normalise the data on the x axis by
      -- - filling gaps (if int, bigint, numeric, timestamp, timestamptz)
      -- - scaling the x axis to "width"
      true as normalise_x
  ),
  
  -- The rest doesn't need to be touched
  --------------------------------------
  
  -- Pre-calculated dimensions of the source data
  source_dimensions (kmin, kmax, kstep, vmin, vmax) as (
    select min(key), max(key), (max(key) - min(key)) / max(width), min(value), max(value)
    from source, constants
  ),
  
  -- Normalised data, which fills the gaps in case the key data type can be
  -- generated with generate_series (int, bigint, numeric, timestamp, timestamptz)
  source_normalised (key, value) as (
    select k, coalesce(sum(source.value), 0)
    from source_dimensions
      cross join constants
      cross join lateral generate_series(kmin, kmax, kstep) as t (k)
      left join source on source.key >= t.k and source.key < t.k + kstep
    group by k
  ),

  -- Replace source_normalised by source if you don't like the normalised version
  actual_source (i, key, value) as (
    select row_number() over (order by key), key, value 
    from source_normalised, constants
    where normalise_x
    union all
    select row_number() over (order by key), key, value
    from source, constants
    where not normalise_x
  ),
    
  -- Pre-calculated dimensions of the actual data
  actual_dimensions (kmin, kmax, kstep, vmin, vmax, width_or_count) as (
    select 
      min(key), max(key), (max(key) - min(key)) / max(width), min(value), max(value), 
      case
        when every(normalise_x) then least(max(width), count(*)::int) 
        else count(*)::int 
      end
    from actual_source, constants
  ),
  
  -- Additional convenience
  dims_and_consts as (
    with 
      temp as (
        select *, 
        (length(characters) + length(separator)) * width_or_count as bar_width
      from actual_dimensions, constants
    )
    select *,
      (bar_width - length(kmin::text) - length(kmax::text)) as x_label_pad
    from temp
  ),
  
  -- A cartesian product for all (x, y) data points
  x (x) as (select generate_series(1, width_or_count) from dims_and_consts),
  y (y) as (select generate_series(1, height) from dims_and_consts),

  -- Rendering the ASCII chart
  chart (rn, chart) as (
    select
      y,
      lpad(y * (vmax - vmin) / height || '', label_pad) 
        || ' | ' 
        || string_agg(
             case 
               when height * actual_source.value / (vmax - vmin) >= y then characters 
               else repeat(' ', length(characters)) 
             end, separator 
             order by x
           )
    from 
      x left join actual_source on actual_source.i = x, 
      y, dims_and_consts
    group by y, vmin, vmax, height, label_pad
    union all
    select 
      0, 
      repeat('-', label_pad) 
        || '-+-' 
        || repeat('-', bar_width)
    from dims_and_consts
    union all
    select 
      -1, 
      repeat(' ', label_pad) 
        || ' | ' 
        || case 
             when x_label_pad < 1 then '' 
             else kmin || repeat(' ', x_label_pad) || kmax 
           end
    from dims_and_consts
  )
select chart
from chart
order by rn desc
;
