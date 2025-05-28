module Sample.SqaDemo
import Data.String
import Data.Maybe
import Data.List
import Data.List.Elem
import Data.Vect
import Records

interface HandlerImpl handler where
  handlerLabel : String
  handlerInputType : Type
  handlerDefault : Maybe handlerInputType

record Filter  where
  constructor MkFilter
  type : Type
  description : String

Handler : Type
Handler = Type

Handlers : Type
Handlers = List Handler

record Param where
  constructor MkParam
  type : Type
  description : String
  
data Method = 
  Get | Post | Put | Patch | Head

data PathEntry = 
  PathLabel String |
  Capture String Type

data Body = HasBody (List Type) Type
          | NoBody
    
data Response = Ok (List Type) Type
              | Created 
              | NoContent
              | Accepted
    
Path : Type
Path = List PathEntry  

implementation FromString PathEntry where
  fromString = PathLabel

-- create a spec for handlers bundled with it's implementation.
HandlerRecordSpec : (ts:List Handler) -> 
                    AllConstraints HandlerImpl ts => 
                    RecordSpec (h ** HandlerImpl h)
HandlerRecordSpec [] @{()} = []
HandlerRecordSpec (t::ts) @{(c,cs)} = 
  (handlerLabel @{c}, (t ** c)) :: HandlerRecordSpec ts @{cs}

HandlerSpec : (t ** HandlerImpl t) -> Type
HandlerSpec (h ** c) = handlerInputType @{c}

record EndPointSpec (handlers : List Handler) where
  constructor MkEndPointSpec
  method : Method
  summary : String
  description : String
  {default NoBody body : Body}
  response : Response
  path : Path
  { auto handlerImpls : AllConstraints HandlerImpl handlers }
  {default [] params : RecordSpec Param}
  handlerSpecs : Record (HandlerRecordSpec handlers) (HandlerSpec . FieldSpec)

ApiSpec : Handlers -> Type
ApiSpec handlers = RecordSpec $ EndPointSpec handlers

CaptureRecordSpec : Path -> RecordSpec Type
CaptureRecordSpec [] = []
CaptureRecordSpec (Capture label tp :: xs) = 
  (label, tp) :: CaptureRecordSpec xs
CaptureRecordSpec (_ :: xs) = CaptureRecordSpec xs

CaptureType : Path -> Type -> Type
CaptureType p tp with (CaptureRecordSpec p)
  _ | [] = tp
  _ | specs = SimpleRecord specs -> tp

ParametersType : RecordSpec Param -> Type -> Type
ParametersType [] tp = tp
ParametersType p tp = Record p (type . FieldSpec) -> tp

BodyType : Body -> Type -> Type
BodyType NoBody ret = ret
BodyType (HasBody _ tp) ret = tp -> ret

ResponseType : Response -> Type
ResponseType (Ok _ ret) = ret
ResponseType _ = ()

EndPointServer : (m:Type -> Type) -> Monad m => EndPointSpec handlers -> Type
EndPointServer m ep = 
  CaptureType ep.path $ ParametersType ep.params $ BodyType ep.body $ m $ ResponseType ep.response


data JSON : Type where
data LocalTime : Type where

data FilterHandler : Type where
data PaginationHandler : Type where

HandlerImpl FilterHandler where
  handlerLabel = "filters"
  handlerInputType = RecordSpec Filter
  handlerDefault = Just []

HandlerImpl PaginationHandler where
  handlerLabel = "pagination"
  handlerInputType = Bool
  handlerDefault = Just False

PetShopHandlers : Handlers
PetShopHandlers = [FilterHandler, PaginationHandler]

PetShopApi : ApiSpec PetShopHandlers
PetShopApi = 
  [ ("getPets",
     MkEndPointSpec 
     { method = Get
     , path = ["pets", Capture "id" Int]
     , summary = "Get the list of pets"
     , description = "Get the list of pets"
     , params = 
       [ ("name"
         , MkParam { type = Maybe String
                   , description = "search on part of the name"})
       , ("born"
         ,MkParam { type = Maybe LocalTime
                  , description = "birth date"})
       , ("species"
         ,MkParam { type = Maybe String
                  , description = "species"})
       , ("accessories"
         ,MkParam { type = Bool
                  , description = "if true, show accessories"
                  })]
     , response = Ok [JSON] String
     , handlerSpecs = 
       [ ( "filters" :-> 
            [( "name"
             , MkFilter { type = String
                        , description = "match part of the pet name"})])
       , ( "pagination" :-> True)]})]

getPets : EndPointServer IO $ fromJust $ lookup "getPets" PetShopApi
getPets captures params =
  if get "accessories" params
  then pure ?ts
  else pure "don't use it"

PetShopServer : Record PetShopApi $ EndPointServer IO . FieldSpec
PetShopServer =
  [ "getPets" :-> getPets
  ]

-- Local Variables:
-- idris-load-packages: ("records" "contrib")
-- End: