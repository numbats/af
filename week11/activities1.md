1.  Fit a regression model with a piecewise linear trend with Fourier terms for the US leisure employment data.
    
    ```r
    leisure <- us_employment |>
      filter(
        Title == "Leisure and Hospitality",
        year(Month) > 2001
      ) |>
      mutate(Employed = Employed / 1000) |>
      select(Month, Employed)
    ```

2. Does the model fit well? What are the implications for forecasting?
