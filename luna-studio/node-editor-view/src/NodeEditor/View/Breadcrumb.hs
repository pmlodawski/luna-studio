{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module NodeEditor.View.Breadcrumb where

import           Common.Prelude

import qualified Control.Lens.Aeson                 as Lens
import qualified NodeEditor.React.Model.Breadcrumbs as B
import           NodeEditor.React.Model.Breadcrumbs (Breadcrumb, BreadcrumbItem, Named)
import qualified NodeEditor.React.Model.App         as App

import Common.Data.JSON                   (toJSONVal)
import Data.Aeson                         (ToJSON (toEncoding, toJSON))
import Data.Convert                       (Convertible (convert))
import NodeEditor.React.Model.App         (App)
import NodeEditor.View.Diff               (DiffT, diffApply)


data BreadcrumbView = BreadcrumbView
    { _moduleName :: Maybe String
    , _items      :: [Named (Breadcrumb BreadcrumbItem)]
    } deriving (Eq, Generic, Show)

makeLenses ''BreadcrumbView

instance ToJSON BreadcrumbView where
    toEncoding = Lens.toEncoding
    toJSON = Lens.toJSON

instance Convertible App BreadcrumbView where
    convert a = BreadcrumbView
        {- modeuleName -} (a ^. App.moduleName)
        {- items       -} (a ^. App.breadcrumbs . to B.namedInits)

foreign import javascript safe "callback.getNodeEditorView().setBreadcrumb($1)"
    setBreadcrumb__ :: JSVal -> IO ()

setBreadcrumb :: MonadIO m => BreadcrumbView -> m ()
setBreadcrumb = liftIO . setBreadcrumb__ <=< toJSONVal

breadcrumbView :: MonadIO m => DiffT BreadcrumbView m ()
breadcrumbView = diffApply setBreadcrumb
