
1. For the following series, find an appropriate Box-Cox transformation in order to stabilise the variance.

    * United States GDP from `global_economy`
    * Slaughter of Victorian “Bulls, bullocks and steers” in `aus_livestock`
    * Victorian Electricity Demand from `vic_elec`.
    * Gas production from `aus_production`

2. Why is a Box-Cox transformation unhelpful for the `canadian_gas` data?

3. Produce the following decomposition

    ```r
    canadian_gas |>
      STL(Volume ~ season(window=7) + trend(window=11)) |>
      autoplot()
    ```

4. What happens as you change the values of the two `window` arguments?

5. How does the seasonal shape change over time? [Hint: Try plotting the seasonal component using `gg_season`.]

6. Can you produce a plausible seasonally adjusted series? [Hint: `season_adjust` is one of the variables returned by `STL`.]
