library (Inferelator)
library (RUnit)
#------------------------------------------------------------------------------------------------------------------------
run.tests = function ()
{
  test.emptyCtor ()
  test.loadLocalYamlFile ()
  test.loadPackageYamlFile ()
  test.loadBogusFileAndPackageNames ()

  test.updateScalar ()
  test.updateVector ()

} # test.params
#------------------------------------------------------------------------------------------------------------------------
test.emptyCtor = function ()
{
  print ('--- test.emptyCtor')
  params <- Parameters ()
  checkEquals (length (categories (params)), 0)

} # test.emptyCtor
#------------------------------------------------------------------------------------------------------------------------
test.emptyCtor = function ()
{
  print ('--- test.emptyCtor')
  params <- Parameters ()
  checkEquals (length (categories (params)), 0)

} # test.emptyCtor
#------------------------------------------------------------------------------------------------------------------------
test.loadLocalYamlFile = function ()
{
  print ('--- test.loadLocalYamlFile')
  filename <- file.path (find.package ('Inferelator'), 'extdata', 'defaultParams.yml')
  stopifnot (file.exists (filename))
  params <- Parameters (filename)
  checkEquals (value (params, 'general', 'priorWeight'), c (0.01, 1))
  
} # test.loadLocalYamlFile
#------------------------------------------------------------------------------------------------------------------------
test.loadBogusFileAndPackageNames = function ()
{
  print ('--- test.loadBogusFileAndPackageNames')
  params <- Parameters (file.path ('bogusDirectory', 'bogusFile.yml'))
  checkTrue (is.null (params))
  params <- Parameters ('BogusPackage')
  checkTrue (is.null (params))

} # test.loadBogusFileAndPackageNames
#------------------------------------------------------------------------------------------------------------------------
test.loadPackageYamlFile = function ()
{
  print ('--- test.loadPackageYamlFile')
  params <<- Parameters ('Inferelator')
  checkEquals (length (categories (params)), 5)
  checkEquals (value (params, 'general', 'priorWeight'), c (0.01, 1))
  
} # test.loadLocalYamlFile
#------------------------------------------------------------------------------------------------------------------------
test.updateScalar = function ()
{
  print ('--- test.updateScalar')
  params <- Parameters ('Inferelator')
  checkEquals (value (params, 'clr', 'numGenes'), 0)
  value (params, 'clr', 'numGenes') <- 3
  checkEquals (value (params, 'clr', 'numGenes'), 3)
  
} # test.updateScalar
#------------------------------------------------------------------------------------------------------------------------
test.updateVector = function ()
{
  print ('--- test.updateVector')
  params <- Parameters ('Inferelator')
  checkEquals (value (params, 'general', 'priorWeight'), c (0.01, 1.0))
  value (params, 'general', 'priorWeight') <-  c (33, 0.01, 1.0, 55)  
  checkEquals (value (params, 'general', 'priorWeight'), c (33, 0.01, 1.0, 55.0))

} # test.updateVector
#------------------------------------------------------------------------------------------------------------------------
