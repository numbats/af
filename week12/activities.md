1.  Fit a regression model with a piecewise linear trend and Fourier terms for the US leisure employment data.
    
    ```r
    leisure <- us_employment |>
      filter(Title == "Leisure and Hospitality", year(Month) > 2001) |>
      mutate(Employed = Employed / 1000) |>
      select(Month, Employed)
    ```

2. Add a dynamic regression model with the same predictors.
3. How do the models compare on AICc?
4. Does the additional ARIMA component fix the residual autocorrelation problem in the regression model?
5. How different are the forecasts from each model?
