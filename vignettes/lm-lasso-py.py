#!/usr/bin/env python
# coding: utf-8
# reference from textbook 'A computational approach to statistical learning' page 189-191

import numpy as np


def soft_thresh_py(a,b):
  '''Soft threshold function in Python.
  Args: 
   a - Numeric vector of values to threshold
   b - The soft thresholded value
  Returens:
    Numeric vector of the soft-thresholded values of a.
    '''
  import numpy as np
  if abs(a) <= b :
    a = 0.0
  if a > b:
    a = a - b
  if a < -b:
    a= a+b
  return a
  

def beta_update_py(X, Y, lamb,b):
  '''Update beta vector using coordinate descent in Python.
  Args: 
    X - A numeric data matrix
    Y - Respinse vector
    lamb - The penalty term
    b - A vector of warm start coefficients for the algorithm
    
  Returns:
    Regression vectors with ncol(X) columns
    '''
  n=len(Y)
  W=1/n
  WX = W*X
  WX2 = W*X*X
  Xb = X @ b
  for i in range(len(b)):
       Xb=Xb-X[:,i].reshape(n,1)*float(b[i])
       b[i]= soft_thresh_py(sum(WX[:,i]*np.subtract(Y, Xb))[0],lamb)
       b[i] = b[i]/sum(WX2[:,i])
       Xb = Xb + X[:,i].reshape(n,1)*b[i]
  return b
  

def lm_lasso_py(X,Y,l, maxit=50,tol=0.00001):
  '''Compute linear elastic net using coordinate descent
  Args: 
    X - A numeric data matrix
    Y - Response vector
    l - The penalty term
    maxit - Integer maximum number of iterations
    tol - Numeric tolerance parameter
  Returns:
    Regression vector beta of length ncol(X)
  '''
  b=np.array([[0.0,0.0,0.0,0.0]]).T
  for j in range(1000):
    b_old=b.copy()
    b = beta_update_py(X,Y,l, b)
    diff = sum(np.absolute(np.subtract(b, b_old)))[0]
    if diff <= tol:
        print(j)
        return b
    elif j ==maxit-1:
        print("Function did not converge")
        return b

      


