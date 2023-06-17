{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Routes
    ( routes
    ) where

import           AST                         (Expr, Value, parseExpr)
import           CLI                         (satisfyInterPure)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Map                    (Map, singleton)
import           Data.Maybe                  (mapMaybe, fromMaybe)
import           GHC.Generics                (Generic)
import qualified HotDrinkF                   as HD
import           Network.Wai                 (Middleware)
import           Network.Wai.Middleware.Cors
import           Text.Megaparsec             (parse)
import qualified WarmDrinkF                  as WD
import           Web.Scotty
import qualified Data.Map as Map
import Network.HTTP.Types.Status (status500)
import Control.Monad.IO.Class (liftIO)

data JSONVariable
  = JSONVariable
      { varID    :: String
      , varValue :: Value
      }
  deriving (FromJSON, Generic, ToJSON)

jsonVarToVar :: JSONVariable -> Map String (Maybe Value)
jsonVarToVar (JSONVariable ident value) = singleton ident (Just value)

data JSONComponent
  = JSONComponent
      { compID    :: Int
      , variables :: [JSONVariable]
      , strength  :: [String]
      }
  deriving (FromJSON, Generic, ToJSON)

-- TOOD: add strength
jsonCompToComp :: [HD.Constraint] -> JSONComponent -> Maybe WD.Component
jsonCompToComp cs (JSONComponent ident vs strg) = do
    let varMap = foldr (\v m -> jsonVarToVar v <> m) mempty vs
    return $ WD.Component ident varMap cs strg


data JSONMethod
  = JSONMethod
      { methodName  :: String
      , inputs      :: [String]
      , expressions :: [(String, String)]
      }
  deriving (FromJSON, Generic, ToJSON)

jsonExprToExpr :: String -> Maybe Expr
jsonExprToExpr expr =
    case parse parseExpr "" expr of
        Left _  -> Nothing
        Right e -> Just e

jsonMethodToMethod :: JSONMethod -> Maybe HD.MethodGraph
jsonMethodToMethod (JSONMethod name ins exprs) =
    let parsedExprs = mapMaybe (jsonExprToExpr . snd) exprs
        met = (name, zip (map fst exprs) parsedExprs)
        mg = HD.methodToGraph ins met
    in (if length parsedExprs == length exprs then Just mg else Nothing)

data JSONConstraint
  = JSONConstraint
      { constraintName :: String
      , methods        :: [JSONMethod]
      }
  deriving (FromJSON, Generic, ToJSON)

jsonConstraintToConstraint :: JSONConstraint -> Maybe HD.Constraint
jsonConstraintToConstraint (JSONConstraint _ ms) = do
    methodGraphs <- mapM jsonMethodToMethod ms
    return $ HD.Constraint methodGraphs

data Input
  = Input
      { components               :: [JSONComponent]
      , constraints              :: [JSONConstraint]
      , intercalatingConstraints :: [JSONConstraint]
      , enforceFromIndex         :: Int
      }
  deriving (FromJSON, Generic, ToJSON)

data Output where
  Output :: {vars :: Map String [JSONVariable]} -> Output
  deriving (FromJSON, Generic, ToJSON)

myCors :: Middleware
myCors = cors (const $ Just policy)
    where
      policy = CorsResourcePolicy
          { corsOrigins = Nothing
          , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
          , corsRequestHeaders = simpleHeaders
          , corsExposedHeaders = Nothing
          , corsMaxAge = Nothing
          , corsVaryOrigin = False
          , corsRequireOrigin = False
          , corsIgnoreFailures = False
          }

convertToJSONVar :: Map String (Maybe Value) -> [JSONVariable]
convertToJSONVar = map (\(k, v) -> JSONVariable k (fromMaybe (error "Nothing") v)) . Map.toList

routes :: ScottyM ()
routes = do
    middleware myCors
    post "/" $ do
        input <- jsonData :: ActionM Input
        let cs = mapMaybe jsonConstraintToConstraint (constraints input)
            ics = mapMaybe jsonConstraintToConstraint (intercalatingConstraints input)
            comps = mapMaybe (jsonCompToComp cs) (components input)
            componentList = WD.ComponentList comps ics
            satisfied = satisfyInterPure (WD.intercalatingConstraints componentList) (drop (enforceFromIndex input) $ WD.components componentList)
        case satisfied of
            Just cmps -> do
                json $ Output $ Map.fromList $ map (\c -> (show $ WD.identifier c, convertToJSONVar $ WD.variables c)) (take (enforceFromIndex input) (WD.components componentList) ++ cmps)
            Nothing -> do
                status status500
