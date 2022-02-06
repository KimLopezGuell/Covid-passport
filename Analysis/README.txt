!! CAREFUL: The current analyses are being made with incidence ratios instead of "raw numbers". Yet this new methodology has only been implemented so far to UK cases and hospital admissions. The rest of the models are outdated as of now.

For the moment, the "Analysis" folder only contains negative binomial regression models on Denmark, Germany France, UK and Catalonia cases, hospital admissions, vaccines and stratified deaths data; and ARIMA modeling for cases and admissions in the UK and in Denmark (this last one only for the 4th and 5th change). For some of these deaths are not yet available. This last stratification is done on two pairs of groups: male and female, young (<60yrs) and old (>60 yrs). All the analyses are done with both raw and 7-day averaged smoothed data of the outcomes, if possible. Vaccines are separated into first and second doses if possible.

As the models were nonsensical for time intervals with several monotony and convexity changes, each Covid Passport intervention change in intensity has been isolated and is modelled in its local time interval independently of the others. The time interval has been chosen such that there is no monotony change in either side of the intervention time point, conditional on having 10 points at either side of the interval (intervention, intervention+lag). If possible, the interval is extended as much as the pattern allows. 

"NA" values have been added for negative incorrect data points. A lag of 5 days has been taken for cases, of 7 days for hospital admissions, of 0 days for vaccines and of 19 days for deaths.

The following plots and files can be found:

"Tables_SR.ods" : Document with summary of all the models computed so far. For each "Country/Region_Code CP#⅛ (Country/Region_Code: De, Fr, Dk, Wa, Sc, En, NI, Cat) it has information on:
- Coef t2: Coefficient of the change in tendency after the intervention+lag for the negative binomial regression model.
- p-value coef: Its associated p-value
- Coef NPI: Coefficient of the change in level after the intervention+lag for the negative binomial regression model.
- p-value coef: Its associated p-value
- p-value DW: P-value of the Durbin-Watson test on the residuals.
- Intervals: Number of points at the left of the intervention date and right of the intervention+lag date, respectively, taken for the modelling of the particular change in Covid Passport intervention.
We expect a decrease in the number of cases, deaths and in hospital admissions and an increase in vaccinations when the restrictivity of the Covid Passport intervention increases. Hence, each column is painted in green if its coefficient aligns with that expectation and in red if it does not.
Data on DID tests (explained below) are also included.
ARIMA information on coefficients, L-Jung correlation tests and variable (p,d,q) selection are also included for the previously mentioned modeled outcomes and countries.

For each country, CP# means the # change of the Covid Passport intervention.
In each COUNTRY/intervention/CP* folder:
- "COUNTRY images_CP#" : Plot of the cases for the selected time interval of the intervention change + Fitted values at each side of the intervention/lag for the BNreg model + Predicted values without the intervention.
- "glm_output.txt" : Model summary, coefficients and exp(coefficients), Durbin-Watson test of the residuals which show in general the need to take autocorrelation into account.
- "Model_NPreg_residuals_CP*" : Residual plots for the models.
In each COUNTRY/ARIMA/CP* folder:
- "ARIMA_country_output_CP*.jpg" has the residual plot
- "ARIMA_country_output_CP*.txt" has information on the dataset, the summary of the model and the L-Jung test for correlation on the residuals

The folder "Plots_nice + Bootstrap" contains plots for the last two interventions in Denmark, as well as all the interventions in Catalunya, for cases and hospital admissions. All the plots have in orange or green the tendencies of the interventions, straight for pre-values and dashed for post-values. In red shadow frames of the interventions are marked. The grey shadows mark the confidence intervals for the predicted and forecasted values. It also contains bootstrap error information on these models whenever computable.

The folder "Catalunya_separate" contains two sub-folders, "PCR_Cat_cases" and "TAR_Cat_cases". Both of them have a "Cases_plot.jpg" plot of cases in Catalonia for the last part of 2021 divided by total number, Delta number and Omicron number of cases, per each testing method. They also have plots and information on SR modeling for the data. They have preliminar ARIMA models - but these should not be taken into account as the data is smoothed and thus has too much inherent correlation.

The folder "DID" contains text files of differences in differences models for England vs Wales, Scotland and NI for their respective Covid Passport interventions modelled in SR and ARIMA. Data and summary of the models are provided. Plots comparing England and other regions cases, with proportion confidence intervals included, are provided as well. They have been moved so that their outcome on the intervention date is the same. Therefore, the y axis is not reliable for both lines in these plots.

The folder "UK paper" contains the selected data and images for the UK paper currently being written.
