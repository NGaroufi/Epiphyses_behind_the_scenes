# -*- coding: utf-8 -*-

import numpy as np


from model.MSVR import MSVR

msvr = MSVR(kernel = 'linear', C = 1)

# Train
msvr.fit(r.train_input, r.train_target)

# Predict with test set
testPred = msvr.predict(r.test_input)
