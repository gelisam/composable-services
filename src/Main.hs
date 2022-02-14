{-# LANGUAGE DataKinds, GADTs, KindSignatures, PolyKinds, TypeOperators #-}
module Main where

import Control.Monad.Free
import Data.Kind


data EndpointApi
  = MkEndpointApi Type Type

type Api
  = [EndpointApi]


data Elem
       (x :: k)
       (xs :: [k])
     where
  Here
    :: Elem x (x ': xs)
  There
    :: Elem x xs
    -> Elem x (y ': xs)

data EffectF
       (required :: Api)
       (a :: Type)
     where
  Call
    :: Elem ('MkEndpointApi i o) required
    -> i
    -> (o -> a)
    -> EffectF required a

type Effect required
  = Free (EffectF required)


data EndpointImpl
       (required :: Api)
       (endpoint :: EndpointApi)
     where
  MkEndpointImpl
    :: (i -> Effect required o)
    -> EndpointImpl required ('MkEndpointApi i o)

data ServiceApi = MkServiceApi
  { serviceRequired :: Api
  , serviceProvided :: Api
  }

data ServiceImpl
       (service :: ServiceApi)
     where
  ServiceNil
    :: ServiceImpl ('MkServiceApi required '[])
  ServiceCons
    :: EndpointImpl required endpoint
    -> ServiceImpl ('MkServiceApi required endpoints)
    -> ServiceImpl ('MkServiceApi required (endpoint ': endpoints))

data ServicesImpl
       (services :: [ServiceApi])
     where
  ServicesNil
    :: ServicesImpl '[]
  ServicesCons
    :: ServiceImpl service
    -> ServicesImpl services
    -> ServicesImpl (service ': services)


data SystemApi = MkSystemApi
  { systemRequired :: Api
  , systemProvided :: Api
  }

data InterfaceIsImplemented
       (services :: [ServiceApi])
       (system :: SystemApi)
       (interface :: EndpointApi)
     where
  ImplementedInternally
    :: Elem ('MkServiceApi required provided) services
    -> Elem interface provided
    -> InterfaceIsImplemented services system interface
  ImplementedElsewhere
    :: (system ~ 'MkSystemApi required provided)
    => Elem endpoint required
    -> InterfaceIsImplemented services system interface

data DependenciesAreSatisfied
       (services :: [ServiceApi])
       (system :: SystemApi)
       (endpoints :: Api)
     where
  DependenciesAreSatisfiedNil
    :: DependenciesAreSatisfied services system '[]
  DependenciesAreSatisfiedCons
    :: InterfaceIsImplemented services system endpoint
    -> DependenciesAreSatisfied services system endpoints
    -> DependenciesAreSatisfied services system (endpoint ': endpoints)

data InnerConsistency
       (allServices :: [ServiceApi])
       (system :: SystemApi)
       (services :: [ServiceApi])
     where
  InnerConsistencyNil
    :: InnerConsistency allServices system '[]
  InnerConsistencyCons
    :: (service ~ 'MkServiceApi required provided)
    => DependenciesAreSatisfied allServices system required
    -> InnerConsistency allServices system services
    -> InnerConsistency allServices system (service ': services)

data OuterConsistency
       (services :: [ServiceApi])
       (system :: SystemApi)
       (interfaces :: Api)
     where
  OuterConsistencyNil
    :: OuterConsistency services system '[]
  OuterConsistencyCons
    :: InterfaceIsImplemented services system interface
    -> OuterConsistency services system interfaces
    -> OuterConsistency services system (interface ': interfaces)

data SystemImpl
       (system :: SystemApi)
     where
  MkSystemImpl
    :: (system ~ 'MkSystemApi required provided)
    => ServicesImpl services
    -> InnerConsistency services system services
    -> OuterConsistency services system provided
    -> SystemImpl system
  

main :: IO ()
main = putStrLn "typechecks."
