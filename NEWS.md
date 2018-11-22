IBCF.MTME v1.5 (Release date: 2018-11-22)
==============

* Fixed issue with the predictions in the pm$Data.Obs_Pred output.
* Fixed issue with the Cross-validation with two or more traits to use as testing.


IBCF.MTME v1.4 (Release date: 2018-10-22)
==============

* `MSEP` was changed to `MAAPE`.
* `print` function was added.
* `ORCID` was added to the authors of the package.

IBCF.MTME v1.3 (Release date: 2018-06-08)
==============

Changes:
* `IBCF.Years()` now has `colID` parameter to select the identifiers of the observations.
* Fixed a bug caused by the sequence in a for cycle.

IBCF.MTME v1.2 (Release date: 2018-02-19)
==============

Changes:

* Fixed important issue from `IBCF()$predictions_Summary`, now the correlation only uses the predicted testing data.
* Now `IBCF()` function show `$yHat` that is an average of the prediction values of every partition.
* Now `IBCF()` function shows in `$predicted_Partition` all the partitions values predicted (before was `$Predictions` and was changed to not confuse with `$yHat`).
* Now `IBCF()` function shows in `$observed` all the response values from the DataSet.
* Now `IBCF()` function shows in `$Data.Obs_Pred` all the response and predicted values from the DataSet.
* Now `IBCF.Years()` function shows in `$predicted` all the values predicted.
* Now `IBCF.Years()` function shows in `$observed` the response values for all the years from the Traits selected in `Traits.testing`.
* Now `CV.RandomPart()` shows the lenght in every partition in `$CrossValidation_length`.
* Now `CV.RandomPart()` admits `Traits.testing` to only use a `PTesting` percentage defined of the traits specified in the parameter to be used to fit the model.


IBCF.MTME v1.1-2 (Release date: 2018-02-14)
==============

Changes:

* Initial release