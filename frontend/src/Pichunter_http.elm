module Pichunter_http exposing (..)

import Http exposing (..)
import State exposing (..)

postPicture pictureFile = Http.post 
                          { url = "/api/pictures"
                          , body = Http.multipartBody [ Http.filePart "file" pictureFile ]
                          , expect = Http.expectWhatever UploadedImage}
