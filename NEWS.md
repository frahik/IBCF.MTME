IBCF.MTME v1.2 (Release date: 2018-02-19)
==============

Changes:

* Fixed important issue from `IBCF()$predictions_Summary`, now the correlation only uses the predicted testing data.
* Now `IBCF()` function show `$yHat` that is an average of the prediction values of every partition.
* Now `IBCF()` function shows in `$predicted_Partition` all the test predictions (before was `$Predictions` and was changed to not confuse with `$yHat`).
* Now `IBCF()` function shows in `$observed` all the response values from the DataSet.
* Now `IBCF.Years()` function shows in `$predicted` all the values predicted (before was `Data_Obs_Pred`).
* Now `IBCF.Years()` function shows in `$observed` the response values for all the years from the Traits selected in `Traits.testing` (before was `Data_Obs_Pred`).
* Now `CV.RandomPart()` shows the lenght in every partition in `$CrossValidation_length`.
* Now `CV.RandomPart()` admits `Traits.testing` to only use a `PTesting` percentage defined of the traits specified in the parameter to be used to fit the model.


IBCF.MTME v1.1-2 (Release date: 2018-02-14)
==============

Changes:

* Initial release