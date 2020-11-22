#!/usr/bin/env python
# coding: utf-8

def soft_thresh_py(a,b):
  '''Soft threshold function in Python.
  Args: 
   a - Numeric vector of values to threshold
   b - The soft thresholded value
  Returens:
    Numeric vector of the soft-thresholded values of a.
    '''
  import numpy as np
  if a <= b & a>=-b:
    a = 0
  if a > b:
    a = a - b
  if a < b:
    a+b
  return a



