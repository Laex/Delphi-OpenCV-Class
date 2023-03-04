(*
  This file is part of Delphi-OpenCV-Class project.
  https://github.com/Laex/Delphi-OpenCV-Class

  It is subject to the license terms in the LICENSE file found in the top-level directory
  of this distribution and at https://www.apache.org/licenses/LICENSE-2.0.txt

  Copyright 2021, Laentir Valetov, laex@bk.ru

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*)

{$ifndef OPENCV_BACKGROUND_SEGM_HPP}
{$define OPENCV_BACKGROUND_SEGM_HPP}

{$IF not defined(OPENCV_CORE_HPP)}{$I 'core.inc'}{$ifend}

//namespace cv
//{

//! @addtogroup video_motion
//! @{

Type
(** @brief Base class for background/foreground segmentation. :

The class is only used to define the common interface for the whole family of background/foreground
segmentation algorithms.
 *)
    TBackgroundSubtractor = type TAlgorithm;

    pTBackgroundSubtractor = ^TBackgroundSubtractor;

    TBackgroundSubtractorHelper = record helper for TBackgroundSubtractor
    public
    (** @brief Computes a foreground mask.

    @param image Next video frame.
    @param fgmask The output foreground mask as an 8-bit binary image.
    @param learningRate The value between 0 and 1 that indicates how fast the background model is
    learnt. Negative parameter value makes the algorithm to use some automatically chosen learning
    rate. 0 means that the background model is not updated at all, 1 means that the background model
    is completely reinitialized from the last frame.
     *)
//    CV_WRAP virtual void apply(InputArray image, OutputArray fgmask, double learningRate=-1) = 0;
    procedure apply(const image:TInputArray;const fgmask:TOutputArray; learningRate:double=-1); {$IFDEF USE_INLINE}inline; {$ENDIF}

    (** @brief Computes a background image.

    @param backgroundImage The output background image.

    @note Sometimes the background image can be very blurry, as it contain the average background
    statistics.
     *)
//    CV_WRAP virtual void getBackgroundImage(OutputArray backgroundImage) const = 0;
    procedure getBackgroundImage(const backgroundImage:TOutputArray); {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;


(** @brief Gaussian Mixture-based Background/Foreground Segmentation Algorithm.

The class implements the Gaussian mixture model background subtraction described in @cite Zivkovic2004
and @cite Zivkovic2006 .
 *)
  pBackgroundSubtractorMOG2 = ^TBackgroundSubtractorMOG2;

  TBackgroundSubtractorMOG2 = record
  private
    vft: vftable_func;
  public
    class operator Finalize(var Dest: TBackgroundSubtractorMOG2);
    (* @brief Returns the number of last frames that affect the background model
    *)
    // CV_WRAP virtual int getHistory() const = 0;
    (* @brief Sets the number of last frames that affect the background model
    *)
    // CV_WRAP virtual void setHistory(int history) = 0;

    (* @brief Returns the number of gaussian components in the background model
    *)
    // CV_WRAP virtual int getNMixtures() const = 0;
    (* @brief Sets the number of gaussian components in the background model.

      The model needs to be reinitalized to reserve memory.
    *)
    // CV_WRAP virtual void setNMixtures(int nmixtures) = 0;//needs reinitialization!

    (* @brief Returns the "background ratio" parameter of the algorithm

      If a foreground pixel keeps semi-constant value for about backgroundRatio\*history frames, it's
      considered background and added to the model as a center of a new component. It corresponds to TB
      parameter in the paper.
    *)
    // CV_WRAP virtual double getBackgroundRatio() const = 0;
    (* @brief Sets the "background ratio" parameter of the algorithm
    *)
    // CV_WRAP virtual void setBackgroundRatio(double ratio) = 0;

    (* @brief Returns the variance threshold for the pixel-model match

      The main threshold on the squared Mahalanobis distance to decide if the sample is well described by
      the background model or not. Related to Cthr from the paper.
    *)
    // CV_WRAP virtual double getVarThreshold() const = 0;
    (* @brief Sets the variance threshold for the pixel-model match
    *)
    // CV_WRAP virtual void setVarThreshold(double varThreshold) = 0;
    procedure setVarThreshold(varThreshold: double); {$IFDEF USE_INLINE}inline; {$ENDIF}
    (* @brief Returns the variance threshold for the pixel-model match used for new mixture component generation

      Threshold for the squared Mahalanobis distance that helps decide when a sample is close to the
      existing components (corresponds to Tg in the paper). If a pixel is not close to any component, it
      is considered foreground or added as a new component. 3 sigma =\> Tg=3\*3=9 is default. A smaller Tg
      value generates more components. A higher Tg value may result in a small number of components but
      they can grow too large.
    *)
    // CV_WRAP virtual double getVarThresholdGen() const = 0;
    (* @brief Sets the variance threshold for the pixel-model match used for new mixture component generation
    *)
    // CV_WRAP virtual void setVarThresholdGen(double varThresholdGen) = 0;

    (* @brief Returns the initial variance of each gaussian component
    *)
    // CV_WRAP virtual double getVarInit() const = 0;
    (* @brief Sets the initial variance of each gaussian component
    *)
    // CV_WRAP virtual void setVarInit(double varInit) = 0;

    // CV_WRAP virtual double getVarMin() const = 0;
    // CV_WRAP virtual void setVarMin(double varMin) = 0;

    // CV_WRAP virtual double getVarMax() const = 0;
    // CV_WRAP virtual void setVarMax(double varMax) = 0;

    (* @brief Returns the complexity reduction threshold

      This parameter defines the number of samples needed to accept to prove the component exists. CT=0.05
      is a default value for all the samples. By setting CT=0 you get an algorithm very similar to the
      standard Stauffer&Grimson algorithm.
    *)
    // CV_WRAP virtual double getComplexityReductionThreshold() const = 0;
    (* @brief Sets the complexity reduction threshold
    *)
    // CV_WRAP virtual void setComplexityReductionThreshold(double ct) = 0;

    (* @brief Returns the shadow detection flag

      If true, the algorithm detects shadows and marks them. See createBackgroundSubtractorMOG2 for
      details.
    *)
    // CV_WRAP virtual bool getDetectShadows() const = 0;
    (* @brief Enables or disables shadow detection
    *)
    // CV_WRAP virtual void setDetectShadows(bool detectShadows) = 0;

    (* @brief Returns the shadow value

      Shadow value is the value used to mark shadows in the foreground mask. Default value is 127. Value 0
      in the mask always means background, 255 means foreground.
    *)
    // CV_WRAP virtual int getShadowValue() const = 0;
    (* @brief Sets the shadow value
    *)
    // CV_WRAP virtual void setShadowValue(int value) = 0;

    (* @brief Returns the shadow threshold

      A shadow is detected if pixel is a darker version of the background. The shadow threshold (Tau in
      the paper) is a threshold defining how much darker the shadow can be. Tau= 0.5 means that if a pixel
      is more than twice darker then it is not shadow. See Prati, Mikic, Trivedi and Cucchiara,
      *Detecting Moving Shadows...*, IEEE PAMI,2003.
    *)
    // CV_WRAP virtual double getShadowThreshold() const = 0;

    (* @brief Sets the shadow threshold *)
    // CV_WRAP virtual void setShadowThreshold(double threshold) = 0;

    (* @brief Computes a foreground mask.

      @param image Next video frame. Floating point frame will be used without scaling and should be in range \f$[0,255]\f$.
      @param fgmask The output foreground mask as an 8-bit binary image.
      @param learningRate The value between 0 and 1 that indicates how fast the background model is
      learnt. Negative parameter value makes the algorithm to use some automatically chosen learning
      rate. 0 means that the background model is not updated at all, 1 means that the background model
      is completely reinitialized from the last frame.
    *)
    // CV_WRAP virtual void apply(InputArray image, OutputArray fgmask, double learningRate=-1) CV_OVERRIDE = 0;
    procedure apply(const image: TInputArray;const fgmask: TOutputArray; learningRate: double = -1); {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;
(** @brief Creates MOG2 Background Subtractor

@param history Length of the history.
@param varThreshold Threshold on the squared Mahalanobis distance between the pixel and the model
to decide whether a pixel is well described by the background model. This parameter does not
affect the background update.
@param detectShadows If true, the algorithm will detect shadows and mark them. It decreases the
speed a bit, so if you do not need this feature, set the parameter to false.
 *)
//CV_EXPORTS_W Ptr<BackgroundSubtractorMOG2>
//    createBackgroundSubtractorMOG2(int history=500, double varThreshold=16,
//                                   bool detectShadows=true);
{
    4264
    ?createBackgroundSubtractorMOG2@cv@@YA?AU?$Ptr@VBackgroundSubtractorMOG2@cv@@@1@HN_N@Z
    ?createBackgroundSubtractorMOG2@cv@@YA?AU?$Ptr@VBackgroundSubtractorMOG2@cv@@@1@HN_N@Z
    struct cv::Ptr<class cv::BackgroundSubtractorMOG2> cv::createBackgroundSubtractorMOG2(int,double,bool)
}
function createBackgroundSubtractorMOG2(history: Int = 500; varThreshold: double = 16; detectShadows: BOOL = true): TPtr<TBackgroundSubtractorMOG2>; overload;
external opencv_world_dll name '?createBackgroundSubtractorMOG2@cv@@YA?AU?$Ptr@VBackgroundSubtractorMOG2@cv@@@1@HN_N@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

type
(** @brief K-nearest neighbours - based Background/Foreground Segmentation Algorithm.

The class implements the K-nearest neighbours background subtraction described in @cite Zivkovic2006 .
Very efficient if number of foreground pixels is low.
 *)
    TBackgroundSubtractorKNN = type TBackgroundSubtractor;

    pBackgroundSubtractorKNN = ^TBackgroundSubtractorKNN;

    TBackgroundSubtractorKNNHelper = record helper for TBackgroundSubtractorKNN
    public
    (** @brief Returns the number of last frames that affect the background model
    *)
//    CV_WRAP virtual int getHistory() const = 0;
    (** @brief Sets the number of last frames that affect the background model
    *)
//    CV_WRAP virtual void setHistory(int history) = 0;

    (** @brief Returns the number of data samples in the background model
    *)
//    CV_WRAP virtual int getNSamples() const = 0;
    (** @brief Sets the number of data samples in the background model.

    The model needs to be reinitalized to reserve memory.
    *)
//    CV_WRAP virtual void setNSamples(int _nN) = 0;//needs reinitialization!

    (** @brief Returns the threshold on the squared distance between the pixel and the sample

    The threshold on the squared distance between the pixel and the sample to decide whether a pixel is
    close to a data sample.
     *)
//    CV_WRAP virtual double getDist2Threshold() const = 0;
    (** @brief Sets the threshold on the squared distance
    *)
//    CV_WRAP virtual void setDist2Threshold(double _dist2Threshold) = 0;

    (** @brief Returns the number of neighbours, the k in the kNN.

    K is the number of samples that need to be within dist2Threshold in order to decide that that
    pixel is matching the kNN background model.
     *)
//    CV_WRAP virtual int getkNNSamples() const = 0;
    (** @brief Sets the k in the kNN. How many nearest neighbours need to match.
    *)
//    CV_WRAP virtual void setkNNSamples(int _nkNN) = 0;

    (** @brief Returns the shadow detection flag

    If true, the algorithm detects shadows and marks them. See createBackgroundSubtractorKNN for
    details.
     *)
//    CV_WRAP virtual bool getDetectShadows() const = 0;
    (** @brief Enables or disables shadow detection
    *)
//    CV_WRAP virtual void setDetectShadows(bool detectShadows) = 0;

    (** @brief Returns the shadow value

    Shadow value is the value used to mark shadows in the foreground mask. Default value is 127. Value 0
    in the mask always means background, 255 means foreground.
     *)
//    CV_WRAP virtual int getShadowValue() const = 0;
    (** @brief Sets the shadow value
    *)
//    CV_WRAP virtual void setShadowValue(int value) = 0;

    (** @brief Returns the shadow threshold

    A shadow is detected if pixel is a darker version of the background. The shadow threshold (Tau in
    the paper) is a threshold defining how much darker the shadow can be. Tau= 0.5 means that if a pixel
    is more than twice darker then it is not shadow. See Prati, Mikic, Trivedi and Cucchiara,
    *Detecting Moving Shadows...*, IEEE PAMI,2003.
     *)
//    CV_WRAP virtual double getShadowThreshold() const = 0;
    (** @brief Sets the shadow threshold
     *)
//    CV_WRAP virtual void setShadowThreshold(double threshold) = 0;
end;

(** @brief Creates KNN Background Subtractor

@param history Length of the history.
@param dist2Threshold Threshold on the squared distance between the pixel and the sample to decide
whether a pixel is close to that sample. This parameter does not affect the background update.
@param detectShadows If true, the algorithm will detect shadows and mark them. It decreases the
speed a bit, so if you do not need this feature, set the parameter to false.
 *)
//CV_EXPORTS_W Ptr<BackgroundSubtractorKNN>
//    createBackgroundSubtractorKNN(int history=500, double dist2Threshold=400.0,
//                                   bool detectShadows=true);
{
4263
?createBackgroundSubtractorKNN@cv@@YA?AU?$Ptr@VBackgroundSubtractorKNN@cv@@@1@HN_N@Z
?createBackgroundSubtractorKNN@cv@@YA?AU?$Ptr@VBackgroundSubtractorKNN@cv@@@1@HN_N@Z
struct cv::Ptr<class cv::BackgroundSubtractorKNN> cv::createBackgroundSubtractorKNN(int,double,bool)
}
function createBackgroundSubtractorKNN(history:int=500; dist2Threshold:double=400.0; detectShadows:bool=true):TPtr<TBackgroundSubtractorKNN>;
external opencv_world_dll name '?createBackgroundSubtractorKNN@cv@@YA?AU?$Ptr@VBackgroundSubtractorKNN@cv@@@1@HN_N@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

//4264	?createBackgroundSubtractorMOG2@cv@@YA?AU?$Ptr@VBackgroundSubtractorMOG2@cv@@@1@HN_N@Z	?createBackgroundSubtractorMOG2@cv@@YA?AU?$Ptr@VBackgroundSubtractorMOG2@cv@@@1@HN_N@Z	struct cv::Ptr<class cv::BackgroundSubtractorMOG2> cv::createBackgroundSubtractorMOG2(int,double,bool)

//! @} video_motion

//} // cv

{.$I 'external/opencv.BackgroundSubtractorMOG2.import.inc'}

{$endif}
