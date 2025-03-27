# STATS_EARTH

<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    
</head>
<body>


<h1>The STATS EARTH Extension command</h1>
<p>
This procedure estimates a Multivariate Adaptive Regression Splines (MARS) model.
The MARS algorithm first converts categorical variables into a set of zero-one variables (dummies) referred
to as one-hot coding.  It then fits pairwise linear segments along with the dummies, adding one at each step
to maximally improve the residual sum of squares using hinge functions.  A hinge is defined by
a variable and a knot.
<p>A hinge function for variable x has the form<br>
h(x,c) = max(0, x-c)<br>
or
max(0, c - x)<br>
where c is a constant.
<p>Since h is zero over part of its range, it partitions a variable into two disjoint regions - a hockey stick or hinge.
which might be upside down.
The process of adding hinge functions and one-hot terms (the forward pass) continues until no nontrivial further improvement can
be found.  The result is effects that are linear splines plus the dummies.  This will be obvious from the
variables plot in the output.  A variable may be split multiple times.  That is, it may have multiple knots,
which will be evident from the plot and the coefficients table.
</p>
<p>
This, of course, is likely to overfit the data leading to a lack of generalizeability to new data.
The next step, then, is to remove terms one by one until the best reduced model is found (the backward pass).as measured by
the GCV (Generalized Cross Validation) statistic, which is similar to the AIC (Akaike Information Criterion).
At the other extreme, variables can be constrained to enter linearly or not at all if the linear term
does not improve the GCV. The discovery process can also include interaction terms among the hinges and one-hot
variables if the degree parameter is larger than one.
<p>This process, thus, detects nonlinearities and interactions while selecting the best functions
of the explanatory variables for prediction purposes.  It does not produce traditional significance levels,
since the model has been found by heavily mining the data.</p>
<p>A good source for an introductory exposition of the MARS algorithm and its strengths and weaknesses is the Wikipedia article,<br>
<a href="https://en.wikipedia.org/wiki/Multivariate_adaptive_regression_spline">
Multivariate Adaptive Regression Spline</a>  <br>Specific details on the earth procedure and definitions of the output can be found</br>
<a href="http://www.milbo.org/doc/earth-notes.pdf">Here</a>
<p>
The term MARS is trademarked by the inventor of the algorithms, Jerry Friedman; hence the R procedure uses the name earth, 
and the SPSS procedure is named STATS EARTH.																					
</p>
<p>See the help for the dialog box for more information about the procedure.  This help only defines the syntax,

<h2>Multivariate Adaptive Regression Spline Syntax</h2>
<div class="syntax">
<p>STATS EARTH<br/>
DEPVAR = dependent variable </em><sup>&#42;</sup><br/>
INDVARS = variable list<br/> 
LINEARVARS = variable list<br/>
The union of INDVARS and LINEARVARS must have at least one variable.<br/>
IDVAR = variable - required if predicting<br/>
ESTIMATE = YES<sup>&#42;&#42;</sup> or NO<br/>
PREDICT = NO<sup>&#42;&#42;</sup> or YES<br/>
FAMILY = GAUSSIAN<sup>&#42;&#42;</sup> or BINOMIAL or POISSON, or GAMMA or NONE<br/>
PREDDATASET = name for prediction dataset.  Required if predicting <br/>
PREDTYPE = LINK<sup>&#42;&#42;</sup> or RESPONSE or CLASS</br>
MODELSOURCE = "filespec" for a saved model to use instead of estimating<br/>
DATASOURCE = TRAINING<sup>&#42;&#42;</sup> or NEWDATA<BR/>
SAVEMODEL = "filespec" for saving the estimated model<br/>
</p>

<p>/OPTIONS<br/>
NFOLD = number of folds for cross validation<br/>
DEGREE = order of interaction terms to look for<br/>
MAXTERMS = maximum number of terms allowed in the model</BR>
<p>

<p>/DISPLAY<br/>
MODELPLOTS = YES<sup>&#42;&#42;</sup> or NO<br/>
RESPONSEPLOTS = YES<sup>&#42;&#42;</sup> or NO<br/>
VARIMPPLOT = YES<sup>&#42;&#42;</sup> or NO<br/>
HEIGHT = plot height in inches<br/>
WIDTH = plot height in inches<br/>
FONTSIZE = integer for plot font in points<br/>
</p>


<p><sup>&#42;</sup> Required<br/>
<sup>&#42;&#42;</sup> Default</p>
</div>

<p>STATS EARTH /HELP.  prints this information and does nothing else,

<pre class="example"><code>
STATS EARTH DEPVAR=salary INDVARS=jobcat jobtime prevexp minority 
IDVAR=id ESTIMATE = YES
/DISPLAY HEIGHT=8 WIDTH=8 FONTSIZE=12.
</code></pre>

<h2>Details</h2>
<p>SPLIT FILES is not supported.</p>
<p>Case weights are not supported.</p>
<p>With large datasets - millions of cases and lots of variables - this procedure can take a long time, 
and there is no way to interrupt it.  Setting a small maximum terms limit can dramatically reduce
the time and memory requirements</p>

<h2>Estimation and Prediction</h2>
<strong>ESTIMATE</strong> specifies whether an equation should be estimated.  If NO, a previously estimated model
can be specified via <strong>MODELSOURCE</STRONG> for use in plots and predictions.</p>
<p><p><strong>DEPVAR</strong> specifies the dependent variables for the equation. The measurement level determines
the type of equation but the appropriate family should be chosen.  Only categorical versus scale matters</p>

<p><strong>INDVARS</strong> is the list of categorical and scale independent variables.  Categorical variables
will be recoded into a set of one-hot dummy variables The names of the reocded variables have the form
variable-name-category.  For example, jobcat3 refers to category 3 of the jobcat variable.</p>

<p><strong>LINEARVARS</strong> optionally specifies variables that should only be entered as simple linear terms.
They can appear in the model separately or in interaction terms if selected.  They cannot be categorical variables.</p>

<p><strong>IDVAR</strong> specifies an ID variable, whose values must be distinct.  It is required if making predictions
and must be specified in the estimation phase even if predictions are to be made later.
The variable can be used in merging the prediction dataset back with the input dataset with 
MATCH FILES or similar commands.</p>

<p><strong>PREDICT</strong> specifies whether predictione should be made.  The equation to use
can be either what is estimated if ESTIMATE = YES, or a previously estimated model can be used.
The most recently estimated model is automatically available in the same session when the
command is reinvoked.</p>

<p><strong>MODELSOURCE</strong> specifies a model file previously saved be used for prediction.
If none is specified, the in-memory model is used.</>

<p><strong>DATASOURCE</strong> specifies the input data for predictions.  TRAINING uses the training or estimation
data while NEWDATA specifies that data from the current active dataset be used.  The new data must have
the same variables, coding, and and measurement levels as the estimation data, but only the explanatory variables actually selected
in building the model are required.  Typical usage might be to run with ESTIMATE=YES and PREDICT=NO; then activate the
dataset for which predictions be made or change the case selection and then running with ESTIMATE=NO and PREDICT=YES.</p>

<p><strong>SAVEMODEL</strong> specifies a file for saving the estimated model.</p>

<p><strong>NFOLD</strong> specifies the number of folds for cross validation.  The default
is 0, which means no cross validation.  Typical values would be 5 or 10.</p>

<p><strong>DEGREE</strong> specifies the interaction order to be considered.  The default is 1,
which means no interactions.</p>

<p><strong>MAXTERMS</strong> specifies the maximum number of terms, i.e., coefficients, allowed in
the final model.  By default there is no limit up to 1000 terms.  For large problems,
setting a limit can speed things up considerably.</p>

<h2>Display</h2>
<p><strong>MODELPLOTS</strong> specifies whether the four plots for the overall model are displayed.</p>

<p><strong>RESPONSEPLOTS</strong> specifies whether the response effect plots are displayed.  They
show the shape of the response for each selected variable, whether categorical or scale level.</p>

<p><strong>VARIMPPLOT</strong> specifies whether a plot of the three variable importance measures is
displayed.</p>

<p><strong>HEIGHT</strong> and <strong>WIDTH</strong> specify the dimensions of the plots
in inches.</p>

<p><strong>FONTSIZE</strong> specifie the desired font size in points.  It may, however, not
be effective in some plots.</p>

<h2>Acknowledgements</h2>
<p>The R package citation is<br>
S. Milborrow. Derived from mda:mars by T. Hastie and R. Tibshirani.
earth: Multivariate Adaptive Regression Splines, 2011. R package.
<p>
The R module used by this procedure is the work of
Stephen Milborrow, Trevor Hastie, Rob Tibshirani, Alan Miller, and Thomas Lumley
based on Friedman's papers "Fast MARS" and
"Multivariate Adaptive Regression Splines".
<p>These authors were not involved in the development of the SPSS procedure and
bear no responsibility for any issuses.  The SPSS procedure is the work of
Jon K. Peck
</p>

<p style="font-size:80%;">© Copyright Jon K. Peck 2024</p>
</body>

</html>

