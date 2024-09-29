fun is_older (d1: int*int*int , d2: int*int*int) =
  if #3 d1 < #3 d2 then true
  else if (#3 d1 = #3 d2 andalso #2 d1 < #2 d2) then true
  else if(#2 d1 = #2 d2 andalso #1 d1 < #1 d2) then true
  else false

fun number_in_month (d: (int*int*int) list, m: int) =
  if null d then 0
  else if m = #2(hd d) then number_in_month(tl d, m) + 1
  else number_in_month(tl d, m)

fun number_in_months (d: (int*int*int) list, m: int list) =
  if null m then 0
  else if number_in_month(d, hd m) > 0 then number_in_months(d, tl m) + 1
  else number_in_months(d, tl m)

fun dates_in_month (d: (int*int*int) list, m: int) =
  if null d then []
  else if m = #2(hd d) then (hd d)::dates_in_month(tl d, m)
  else dates_in_month(tl d, m)

fun dates_in_months (d: (int*int*int) list, m: int list) =
  if null m
  then []
  else
    if null (dates_in_month(d, hd m))
    then
      dates_in_months(d, tl m)
    else
      dates_in_month(d, hd m) @ dates_in_months(d, tl m)

fun get_nth (x: string list, n: int) =
  if null x then ""
  else
    if n = 1 then hd x
    else get_nth(tl x, n - 1)

fun date_to_string (d: int*int*int) =
  let
    val months = ["January", "February", "March", "April",
"May", "June", "July", "August", "September", "October", "November", "December"]
  in
    get_nth(months, #2 d) ^ "-" ^ Int.toString(#1 d) ^ "-" ^ Int.toString(#3 d)
  end

fun number_before_reaching_sum(sum:int, l: int list) =
  if sum <= 0 then ~1
  else
    if null l then ~1
    else
      number_before_reaching_sum(sum - hd l, tl l) + 1

fun what_month(d: int) =
  let val daysInMonths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(d, daysInMonths) + 1
  end

fun month_range (day1: int, day2: int) =
  if day1 > day2 then []
  else
    what_month(day1) :: month_range(day1 + 1, day2)

fun oldest (dates: (int*int*int) list) =
  if null dates then NONE
  else
    let fun helper (dates: (int*int*int) list, max) =
      if null(dates) then max
      else
        if is_older(max, hd dates) then helper(tl dates, max)
        else
        helper(tl dates, hd dates)
    in
      SOME (helper(tl dates, hd dates))
    end

fun comulative_sum (numbers: int list) =
  if null numbers then []
  else
  let fun helper(sum: int, numbers: int list) =
    if null numbers then []
    else
      (sum + hd numbers) :: (helper (sum + hd numbers, tl numbers))
  in
    helper (0, numbers)
  end