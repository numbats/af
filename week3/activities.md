
1.  Produce an STL decomposition as follows

    ```r
    us_construction <- us_employment |>
      filter(Title == "Construction", year(Month) > 1980)
    dcmp <- us_construction |>
      model(stl = STL(Employed ~ trend(window = 9) + season(window = 11)))
    dcmp |> components() |> autoplot()
    ```
2. What happens as you change the values of the two `window` arguments?
3. How does the seasonal shape change over time? *[Hint:&nbsp;Try&nbsp;plotting the seasonal component using `gg_season`.]*
4. Can you produce a plausible seasonally adjusted series? *[Hint:&nbsp;`season_adjust` is returned by `components()`.]*
