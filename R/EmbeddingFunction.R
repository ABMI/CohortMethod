# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of CohortMethod
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

createEncoders <- function(data, validationSplit= 0.3,  latentDim = 16L, intermediateThreeDims = c(256L,128L,64L),
                           epochs = 100L, outputFolder = NULL){
  originalDim <- dim(data)[2]

  input_layer <-
    layer_input(shape = c(originalDim))

  encoder <-
    input_layer %>%
    layer_dense(units = intermediateThreeDims[1], activation = "sigmoid") %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = intermediateThreeDims[2], activation = "sigmoid") %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = intermediateThreeDims[3], activation = "sigmoid") %>%
    layer_dense(units = latentDim)

  decoder <-
    encoder %>%
    layer_dense(units = intermediateThreeDims[1], activation = "sigmoid") %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = intermediateThreeDims[2], activation = "sigmoid") %>%
    layer_dropout(rate = 0.1) %>%
    layer_dense(units = intermediateThreeDims[3], activation = "sigmoid") %>%
    layer_dense(units = c(originalDim)) #

  ##compile and train the autoencoder
  autoencoder_model <- keras_model(inputs = input_layer, outputs = decoder)

  autoencoder_model %>% compile(
    loss='mean_squared_error',
    optimizer='adam',
    metrics = c('accuracy')
  )

  ##train onto itself
  history <-
    autoencoder_model %>%
    keras::fit(data,
               data,
               epochs=epochs,
               shuffle=TRUE,
               validation_split = validationSplit
    )
  ##Extract the weights for the encoder
  autoencoder_weights <-
    autoencoder_model %>%
    keras::get_weights()

  ##See the types fo variables of encoder
  #autoencoder_weights %>% purrr::map_chr(class)

  ##Save the weight
  if(!is.null(outputFolder)){
    keras::save_model_weights_hdf5(object = autoencoder_model,filepath = file.path(outputFolder,"autoencoder_weights.hdf5"),overwrite = TRUE)
  }

  ##Load up the weights into an ecoder model and predict
  encoder_model <- keras_model(inputs = input_layer, outputs = encoder)

  #encoder_model %>% keras::load_model_weights_hdf5(filepath = file.path(outputFolder,"autoencoder_weights.hdf5"),skip_mismatch = TRUE,by_name = TRUE)

  encoder_model %>% compile(
    loss='mean_squared_error',
    optimizer='adam',
    metrics = c('accuracy')
  )
  return(list(autoencoder = autoencoder_model, encoder = encoder_model))

}
