require("mlrMBO")


# https://mlrmbo.mlr-org.com/articles/mlrMBO.html 


# Define objective function and its parameters using the package smoof.
# Generate initial design (optional).
# Define mlr learner for surrogate model (optional).
# Set up a MBO control object.
# Start the optimization with mbo().

obj.fun = makeCosineMixtureFunction(1)
obj.fun = convertToMinimization(obj.fun)
print(obj.fun)

obj.fun = makeSingleObjectiveFunction(
  name = "my_sphere",
  fn = function(x) {
    sum(x*x) + 7
  },
  par.set = makeParamSet(
    makeNumericVectorParam("x", len = 2L, lower = -5, upper = 5)
  ),
  minimize = TRUE
)



des = generateDesign(n = 5, par.set = getParamSet(obj.fun), fun = lhs::randomLHS)
des$y = apply(des, 1, obj.fun)

surr.km = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = FALSE))


control = makeMBOControl()
control = setMBOControlTermination(control, iters = 10)
control = setMBOControlInfill(control, crit = makeMBOInfillCritEI())


run = mbo(obj.fun, design = des, learner = surr.km, control = control, show.info = TRUE)

print(run)
