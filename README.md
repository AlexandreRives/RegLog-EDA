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

	fit <- fit_reg_log(formula = class ~ ., data = breast, mode = "batch", learning_rate = 0.01, max_iter = 100, graph = FALSE, espilon = 0.0001)

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
	fit_reg_log(formula = class ~ ., data = train_breast, mode = "batch", 
	    normalize = TRUE, learning_rate = 0.1, max_iter = 1000, cores = 1, 
	    graph = FALSE, epsilon = 1e-05)

	Deviance Residuals :
	      Min      Q1  Median     Q3 Max   Mean
	1 -0.9975 -0.0274 -0.0097 0.0037   1 -7e-04

	Coefficients & Features : 
	  (Intercept)    clump  ucellsize ucellshape mgadhesion     sepics   bnuclei bchromatin   normnucl    mitoses
	1    1.034724 -1.08406 -0.6294483 -0.7943087 -0.4462591 -0.3586972 -1.263574 -0.6778172 -0.4272717 -0.1065407

	Null Deviance :  on 499 degrees of freedom
	Residual deviance :  on 490 degrees of freedom
	AIC :

	Execution time : 0.241096 sec.

	############################################################################################################### 


Then the print function :

	print(fit)

	############################################################################################################### 

	Results of the logistic regression : 
	Call : 
	fit_reg_log(formula = class ~ ., data = train_breast, mode = "batch", 
	    normalize = TRUE, learning_rate = 0.1, max_iter = 1000, cores = 1, 
	    graph = FALSE, epsilon = 1e-05)

	Coefficients & Features : 
	  (Intercept)    clump  ucellsize ucellshape mgadhesion     sepics   bnuclei bchromatin   normnucl    mitoses
	1    1.034724 -1.08406 -0.6294483 -0.7943087 -0.4462591 -0.3586972 -1.263574 -0.6778172 -0.4272717 -0.1065407

	Degrees of Freedom : 499 Total (- intercept); 490 Residual
	Null Deviance :
	AIC :

	Execution time : 0.241096 sec.

	############################################################################################################### 
	
### Mini-batch mode

As the batch mode, you have to use the fit_reg_log function but 2 things are changing :

	fit <- fit_reg_log(formula = class ~ ., data = breast, mode = "mini_batch", batch_size = 10, learning_rate = 0.01, max_iter = 100, graph = FALSE, espilon = 0.0001)
	
You have to change the mode and add the batch_size.
You have obviously an access to the **print()** and **summary()** functions too and to the elements using the $ after the fitted object.

### Online mode

As the batch mode, you have to use the fit_reg_log function :

	fit <- fit_reg_log(formula = class ~ ., data = breast, mode = "online", learning_rate = 0.01, max_iter = 100, graph = FALSE, espilon = 0.0001)

You just have to change the mode and set it "online" to train your dataset.

### Batch parallel mode

As the batch mode, you have to use the fit_reg_log function :

	fit <- fit_reg_log(formula = class ~ ., data = breast, mode = "batch_parallel", learning_rate = 0.01, max_iter = 100, cores = 1, graph = FALSE, espilon = 0.0001)

You just have to change the mode and set it "batch_parallel" to train your dataset.
We advise you to use it if you are planning to fit a big dataset.

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
