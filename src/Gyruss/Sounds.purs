module Gyruss.Sounds where

import Gyruss.Types

import Prelude

import Audio.WebAudio.Types
import Audio.WebAudio.AudioContext
import Audio.WebAudio.DestinationNode
import Audio.WebAudio.Oscillator
import Audio.WebAudio.GainNode
import Audio.WebAudio.AudioParam
import Audio.WebAudio.AudioBufferSourceNode

import Control.Bind
import Control.Monad.Eff
import Control.Monad.ST

import Data.Maybe
import Data.DOM.Simple.Types
import Data.DOM.Simple.Ajax
import Data.DOM.Simple.Events
import Data.DOM.Simple.Window
import DOM

import Debug.Trace

makeSounds :: forall eff. (Eff (wau :: WebAudio, dom :: DOM | eff) Sounds)
makeSounds = do
  context <- makeAudioContext
  return { context:         context
         , musicBuffer:     (Nothing :: Maybe AudioBuffer)
         , fireBuffer:      (Nothing :: Maybe AudioBuffer)
         }

startSounds :: forall eff s. (STRef s State)
            -> (Eff (st :: ST s, wau :: WebAudio, dom :: DOM | eff) Unit)

startSounds st = do
  state <- readSTRef st

  loadAudioBuffer "./shoot.wav" state.sounds.context (\buf -> do
    modifySTRef st (\state -> state { sounds = state.sounds { fireBuffer = Just buf } })
    return unit)

  loadAudioBuffer "./tocata.ogg" state.sounds.context (\buf -> do
    modifySTRef st (\state -> state { sounds = state.sounds { musicBuffer = Just buf } })
--    playBufferedSound state.sounds (Just buf)
    return unit)

loadAudioBuffer :: forall e f.
                   String
                -> AudioContext
                -> (AudioBuffer -> (Eff (wau :: WebAudio | f) Unit))
                -> (Eff (wau :: WebAudio, dom :: DOM | e) Unit)
loadAudioBuffer url ctx cont = do
  req <- makeXMLHttpRequest
  open GET url req
  setResponseType ArrayBuffer req
  addProgressEventListener ProgressLoadEvent (decodeInto ctx req cont) req
  send NoData req

decodeInto :: forall e f.
              AudioContext
           -> XMLHttpRequest
           -> (AudioBuffer -> (Eff (wau :: WebAudio | f) Unit))
           -> DOMEvent
           -> (Eff (wau :: WebAudio, dom :: DOM | e) Unit)
decodeInto ctx req cont ev = do
  audioData <- response req
  case audioData of
    ArrayBufferData abd -> do
      decodeAudioData ctx abd cont traceA
    _ -> traceA "Not an ArrayBufferData"

playBufferedSound :: forall eff. Sounds -> Maybe AudioBuffer
                  -> (Eff (wau :: WebAudio | eff) Unit)

playBufferedSound sounds (Just buf) = do
  node <- createBufferSource sounds.context
  connect node =<< destination sounds.context
  setBuffer buf node
  startBufferSource 0.0 node

playBufferedSound _ _ = return unit
