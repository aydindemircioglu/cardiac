#!/usr/bin/Rscript  --vanilla 

#
# SVMBridge 
#
#		(C) 2015, by Aydin Demircioglu
# 
# SVMBridge is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# SVMBridge is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# Please do not use this software to destroy or spy on people, environment or things.
# All negative use is prohibited.
#


#library (SVMBridge)
	library(devtools)
	load_all (".")

	
# as the libary already loads default wrappers this works
	addSVMPackage (method = "LIBSVM", verbose = FALSE)
	findSVMSoftware ("LIBSVM", searchPath = "../../../svm_large_scale/software/libSVM", verbose = FALSE)

# but this works also
	addSVMPackage (method = "LIBSVM", filePath = "./R/LIBSVM_wrapper.R", softwarePath = "../../../svm_large_scale/software/libSVM", verbose = FALSE)
	


# alternatively, addSVMPackage without softwarePath and search all in a given path
	findAllSVMSoftware (searchPath = "../../../svm_large_scale/software/", verbose = FALSE) 


# wird alle bekannte software-pakete suchen also SVMperf, libSVM, ...
#FIXME: allow path like ~/
	outputAllSVMSoftwarePackages ()


	verbose = FALSE
	
# load iris  for now
	shufflediris = iris[sample(nrow(iris)),]

	trainDataX = data.matrix(shufflediris [1:100,1:4])
	trainDatay = data.matrix(as.numeric(shufflediris [1:100, 5]))
	testDataX = data.matrix(shufflediris [-(1:100),1:4])
	testDatay = data.matrix(as.numeric(shufflediris [-(1:100), 5]))

	# FIXME: for now we only accept binary labels
	trainDatay[trainDatay==3] = 1
	testDatay[testDatay==3] = 1
	trainDatay[trainDatay==2] = -1
	testDatay[testDatay==2] = -1

	
	messagef("\n\n\n======= Train LIBSVM, Traindata from Memory, Model to Memory")
	trainObj =  trainSVM(
		method = "LIBSVM",
		trainDataX = trainDataX, 
		trainDatay = trainDatay, 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = verbose
    )  


	messagef("\n\n\n======= Train LIBSVM, Traindata from File, Model to Memory")
    trainObj =  trainSVM(
		method = "LIBSVM",
		trainDataFile = './tests/data/australian.train',
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = verbose
    )  

    
    messagef("\n\n\n======= Test LIBSVM, Testdata from Disk, Model from Memory, Predictions to Memory")
    testObj =  testSVM(
		method = "LIBSVM",
		testDataFile = './tests/data/australian.train',
        model = trainObj$model,
        verbose = verbose
    )  

    
	messagef("\n\n\n======= Test LIBSVM, Testdata from Memory, Model from Memory, Predictions to Memory")
    testObj =  testSVM(
		method = "LIBSVM",
		testDataX = testDataX, 
		testDatay = testDatay, 
        verbose = verbose,
        model = trainObj$model
    )  

    
    messagef("\n\n\n======= Test LIBSVM, Testdata from Disk, Model from Memory, Predictions to Disk")
    testObj =  testSVM(
		method = "LIBSVM",
		testDataFile = './tests/data/australian.train',
        model = trainObj$model,
        predictionsFile = "./tmp/predictions.txt",
        verbose = verbose
    )  

    
	messagef("\n\n\n======= Test LIBSVM, Testdata from Memory, Model from Memory, Predictions to Disk")
    testObj =  testSVM(
		method = "LIBSVM",
		testDataX = testDataX, 
		testDatay = testDatay, 
        model = trainObj$model,
        predictionsFile = "./tmp/predictions.txt",
        verbose = verbose,
    )  

    
    
    
    
	messagef("\n\n\n======= Train LIBSVM, Traindata from Disk, Model to Disk")
    trainObj =  trainSVM(
		method = "LIBSVM",
		trainDataFile = './tests/data/australian.train',
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = verbose,
        modelFile = "/tmp/libsvm_model.txt"
    )  


    messagef("\n\n\n======= Train LIBSVM, Traindata from Memory, Model to Disk")
    trainObj =  trainSVM(
		method = "LIBSVM",
		trainDataX = trainDataX, 
		trainDatay = trainDatay, 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = verbose,
        modelFile = "/tmp/libsvm_model.txt"
    )  
		    

	messagef("\n\n\n======= Test LIBSVM, Testdata from Memory, Model from Disk, Predictions to Memory")
    testObj =  testSVM(
		method = "LIBSVM",
		testDataX = testDataX, 
		testDatay = testDatay, 
        modelFile = "/tmp/libsvm_model.txt",
		verbose = verbose
    )  

    

    messagef("\n\n\n======= Test LIBSVM, Testdata from Disk, Model from Disk, Predictions to Memory")
    testObj =  testSVM(
		method = "LIBSVM",
		testDataFile = './tests/data/australian.train',
        modelFile = "/tmp/libsvm_model.txt",
        verbose = verbose
    )  
    

    
	messagef("\n\n\n======= Test LIBSVM, Testdata from Memory, Model from Disk, Predictions to Disk")
    testObj =  testSVM(
		method = "LIBSVM",
		testDataX = testDataX, 
		testDatay = testDatay, 
        modelFile = "/tmp/libsvm_model.txt",
        predictionsFile = "./tmp/predictions.txt",
		verbose = verbose
    )  

    

    messagef("\n\n\n======= Test LIBSVM, Testdata from Disk, Model from Disk, Predictions to Disk")
    testObj =  testSVM(
		method = "LIBSVM",
		testDataFile = './tests/data/australian.train',
        modelFile = "/tmp/libsvm_model.txt",
        predictionsFile = "./tmp/predictions.txt",
        verbose = verbose
    )  
    