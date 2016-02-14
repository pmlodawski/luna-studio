module Data.Graph.Model.Cluster where

import Prologue


type family ClusterOf a

class Clustered t where
    clusters :: Lens' t [ClusterOf t]
