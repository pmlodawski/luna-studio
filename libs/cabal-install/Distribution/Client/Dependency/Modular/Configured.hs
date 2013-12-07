module Distribution.Client.Dependency.Modular.Configured where

import Distribution.Client.Types       (OptionalStanza)
import Distribution.PackageDescription (FlagAssignment)

import Distribution.Client.Dependency.Modular.Package

-- | A configured package is a package instance together with
-- a flag assignment and complete dependencies.
data CP qpn = CP (PI qpn) FlagAssignment [OptionalStanza] [PI qpn]
