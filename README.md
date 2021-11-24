# LogRegEDA
The LogRegEDA is a package which give you an easy way to instantiate the logistic regression using the stochastic gradient descent
with three modes :

- Batch mode
- Mini-batch mode
- Online mode

Using this package will allow you to fit, to predict, to see your result and to see an interactive plot of the loss function.

Installing the package
-----------------------

	devtools::install_github("AlexandreRives/RegLog-EDA", dependancies = TRUE)
	
How to use the package ?
------------------------
### Loading the package

First of all, you have to load the package using the **library()** command in your console :

	library(LogRegEDA)
### Importing the dataset

Then, you need to import a dataset. Don't worry, we had your back and you can use the one we put in the package if you first want how the package works :

	breast <- LogRegEDA::breast

Great ! Now you can see on the right that the dataset is loaded. Make a little check with the **head()** function in your console to confirm that the dataset is well loaded.

Now you can be focused on our package.

Fit function
------------------------

Now that everything is loaded and that you have splited your dataset in a train-test. We can launch the fit function.
As we said, we have coded 3 different modes. Let's start with the batch :

### Batch mode

Below, an example on how you have to use it :

	fit <- fit_reg_log(formula = class ~ ., data = breast, mode = "batch", learning_rate = 0.01, max_iter = 100)

One option can be used if you want to scale your dataset :
normalize : FALSE by default.

Using the $ after the fit object can give you an access to 3 lists :
 - the coefficients
 - the costs
 - the residuals

If you want to see the result of your training set, use the **print()** or the **summary()** functions.

Here the summary function :

	summary(fit)
	
	############################################################################################################### 

	Results of the logistic regression : 
	Call : 
	fit_reg_log(formula = class ~ ., data = breast, mode = "batch", learning_rate = 0.01, max_iter = 100)
	
	Coefficients & Features : 
  	(Intercept)       clump  ucellsize ucellshape mgadhesion    sepics    bnuclei bchromatin    normnucl   mitoses
	1   0.9184764 -0.07252196 -0.1751895 -0.1463792 0.01758643 0.1406431 -0.2896429 0.04050536 -0.05746522 0.4983503

	Degrees of Freedom : 698 Total (- intercept); 689 Residual
	Null Deviance :  -6781.664 
	AIC :  1087.696 
	###############################################################################################################


Then the print function :

	############################################################################################################### 

	Results of the logistic regression : 
	Call : 
	fit_reg_log(formula = class ~ ., data = breast, mode = "batch", learning_rate = 0.01, max_iter = 100)

	Coefficients & Features : 
  	(Intercept)       clump  ucellsize ucellshape mgadhesion    sepics    bnuclei bchromatin    normnucl   mitoses
	1   0.9184764 -0.07252196 -0.1751895 -0.1463792 0.01758643 0.1406431 -0.2896429 0.04050536 -0.05746522 0.4983503

	Degrees of Freedom : 698 Total (- intercept); 689 Residual
	Null Deviance :  -6781.664 
	AIC :  1087.696 
	############################################################################################################### 
	
### Mini-batch mode

As the batch mode, you have to use the fit_reg_log function but 2 things are changing :

	fit_reg_log(formula = class ~ ., data = breast, mode = "mini_batch", batch_size = 10, learning_rate = 0.01, max_iter = 100)
	
You have to change the mode and add the batch_size.
You have obviously an access to the **print()** and **summary()** functions too and to the elements using the $ after the fitted object.

### Online mode

As the batch mode, you have to use the fit_reg_log function :

	fit <- fit_reg_log(formula = class ~ ., data = breast, mode = "online", batch_size = 10, learning_rate = 0.01, max_iter = 100)

You just have to change the mode and set it "online" to train your dataset.


Predict function
------------------------

The predict function allows you to use the probability or the class predicted.

### Posterior type

	predict <- predict_reg_log(object = fit, newdata = test, type = "posterior")
	
### Class predicted type

	predict <- predict_reg_log(object = fit, newdata = test, type = "class")


Diverse functions
------------------------

To complete the package, we have provided 2 functions that allows you to encode your dataset :

The first one encode your qualitative data, don't worry and put all your features inside :

	dummies(x = X_data)

The second one encode your target data :

	dummies_y(y = target)
