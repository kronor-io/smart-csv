module Kronor.CircuitBreaker (CircuitBreakerError (..)) where

import RIO


newtype CircuitBreakerError = CircuitBreakerClosed Text
    deriving stock (Show)
    deriving anyclass (Exception)
