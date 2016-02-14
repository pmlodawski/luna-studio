module Data.Graph.Model.Cluster where


type family ClusterOf a

class Clustered t where
    clusters :: Lens' t [ClusterOf t]
