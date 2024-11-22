port module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Lamdera
import Types exposing (..)
import Url
import Time
import Task exposing (Task)
import Markdown

import Html.Attributes as HtmlAttribute

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Events exposing (onClick, onMouseUp, onMouseEnter)

import Time.Extra

port receiveSelection : (String -> msg) -> Sub msg
port requestSelection : () -> Cmd msg
port showTooltip : String -> Cmd msg

type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( {   key = key
        , text = testText
        , selectedText = Nothing
        , feedback = Nothing
        , comment = ""
        , feedbacks = []
      }
    , Lamdera.sendToBackend RequestFeedbacks

    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        MouseUp ->
            ( model, requestSelection() )

        GotSelection text ->
            ( { model | selectedText = Just text }, Cmd.none )
            
        SetSimpleFeedback feedback ->
            ( { model | feedback = Just feedback }, Cmd.none )

        UpdateComment comment ->
            ( { model | comment = comment }, Cmd.none )

        SubmitFeedback ->
            case model.selectedText of
                Just text ->
                    case model.feedback of
                        Just simpleFeedback ->
                            (model, 
                                Time.now
                                    |> Task.perform (\time ->
                                        SendFeedback
                                            { selectedText = text
                                            , timestamp = time
                                            , simpleFeedback = simpleFeedback
                                            , comment = if model.comment /= "" then Just model.comment else Nothing
                                            }
                                    )
                            )
                        Nothing ->
                            -- Maneja el caso en que no se ha seleccionado un feedback simple
                            ( model, Cmd.none )
                Nothing ->
                    -- Maneja el caso en que no hay texto seleccionado
                    ( model, Cmd.none )

        SendFeedback feedbackEntry ->
            ( model
            , Lamdera.sendToBackend (AddFeedback feedbackEntry)
            )

        TriggerTooltip elementId ->
            ( model, showTooltip elementId )


            
updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        UpdateFeedbacks feedbackList ->
            ( { model | feedbacks = feedbackList }, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ layout
            [ width fill, height fill, Background.color (rgb 240 240 240), onMouseUp MouseUp
            , htmlAttribute (HtmlAttribute.attribute "style" "white-space: normal;")
            ]

        <| column
            [ centerX
            , padding 40
            ]
            [ -- Logo de Lamdera
              Element.image [width (px 150)] { src = "https://lamdera.app/lamdera-logo-black.png", description = "Logo Lamdera" }

              -- Mensaje de bienvenida y selección de texto
            , el
                [ padding 40
                ]
                (column []
                    (content model
                        :: case model.selectedText of
                            Just texts ->
                                [ el [] (text ("Has seleccionado: " ++ texts))
                                , row [ spacing 10 ]
                                    [ Input.button
                                        [  ]
                                        { onPress = Just <| SetSimpleFeedback Like
                                        , label = text "Me Gusta"
                                        }
                                    , Input.button
                                        [  ]
                                        { onPress = Just <| SetSimpleFeedback Dislike
                                        , label = text "No Me Gusta"
                                        }
                                    ]
                                , 
                                    template  "Añadir comentario (opcional)" model.comment (\a -> UpdateComment a) 
                                  
                                , el [ onClick SubmitFeedback ] (text "Enviar Feedback")
                                ]
                
                            Nothing ->
                                []
                    )
                )

              -- Lista de feedbacks recibidos
             , feedbackTable model.feedbacks
            ]
        ]
    }

template : String -> String -> (String -> msg) -> Element.Element msg
template title text onChange =
    Input.text
        [ Border.rounded 8 ]
        { text = text
        , onChange = onChange
        , placeholder = Nothing
        , label = Input.labelAbove [ Font.semiBold ] (Element.text title)
        }

feedbackTable : List FeedbackEntry -> Element FrontendMsg
feedbackTable feedbacks =
    column
        [ spacing 20 ]
        [ el [] (text "Feedbacks Recibidos")
        , row
            [ Border.width 1, Border.color (rgb 0.8 0.8 0.8), padding 10 ]
            [ el [ width (fillPortion 1), padding 5, Font.bold,  Border.color (rgb 0.8 0.8 0.8) ] (text "Texto Seleccionado")
            , el [ width (fillPortion 1), padding 5, Font.bold,  Border.color (rgb 0.8 0.8 0.8) ] (text "Feedback")
            , el [ width (fillPortion 1), padding 5, Font.bold,  Border.color (rgb 0.8 0.8 0.8) ] (text "Comentario")
            , el [ width (fillPortion 1), padding 5, Font.bold ] (text "Fecha")
            ]
        , column []
            (List.indexedMap viewFeedback feedbacks)
        ]

viewFeedback : Int -> FeedbackEntry -> Element FrontendMsg
viewFeedback rowId feedbackEntry =
    let
        elementId = \col -> "feedback-" ++ (String.fromInt rowId) ++ "-" ++ col
    in
    row
        [ Border.width 1
        , Border.color (rgba 0 0 0 0.2)
        , padding 10
        , spacing 10
        ]
        [ el
            ([ width (fillPortion 1)
            , Border.color (rgb 0.8 0.8 0.8)
            , padding 5
            , htmlAttribute (HtmlAttribute.attribute "data-full-text" feedbackEntry.selectedText)
            , htmlAttribute (HtmlAttribute.id <| elementId "selectedText")
            , onMouseEnter (TriggerTooltip <| elementId "selectedText")
            ] ++ clipText 150)
            <| (text feedbackEntry.selectedText)

        , el
            ([ width (fillPortion 1)
            , Border.color (rgb 0.8 0.8 0.8)
            , padding 5
            , htmlAttribute (HtmlAttribute.attribute "data-full-text" (simpleFeedbackToString feedbackEntry.simpleFeedback))
            , htmlAttribute (HtmlAttribute.id <| elementId "simpleFeedback")
            , onMouseEnter (TriggerTooltip <| elementId "simpleFeedback")
            ] ++ clipText 150)
            (text (simpleFeedbackToString feedbackEntry.simpleFeedback))

        , el
            ([ width (fillPortion 1)
            , Border.color (rgb 0.8 0.8 0.8)
            , padding 5
            , htmlAttribute (HtmlAttribute.attribute "data-full-text" (case feedbackEntry.comment of
                Just comment -> comment
                Nothing -> "N/A"
            ))
            , htmlAttribute (HtmlAttribute.id <| elementId "comment")
            , onMouseEnter (TriggerTooltip <| elementId "comment")
            ] ++ clipText 150)
            (case feedbackEntry.comment of
                Just comment -> text comment
                Nothing -> text "N/A"
            )

        , el
            ([ width (fillPortion 1)
            , padding 5
            , Border.color (rgb 0.8 0.8 0.8)
            ] ++ clipText 150)
            (text (Time.Extra.toIso8601DateTimeUTC feedbackEntry.timestamp))
        ]

clipText : Int -> List (Attribute msg)
clipText maxWidth = 
    [ Element.htmlAttribute (HtmlAttribute.style "white-space" "nowrap")
    , Element.htmlAttribute (HtmlAttribute.style "overflow" "hidden")
    , Element.htmlAttribute (HtmlAttribute.style "text-overflow" "ellipsis")
    , Element.width (px maxWidth)
    ]

simpleFeedbackToString : SimpleFeedback -> String
simpleFeedbackToString feedback =
    case feedback of
        Like ->
            "Me Gusta"

        Dislike ->
            "No Me Gusta"

        Neutral ->
            "Neutral"

subscriptions : Model -> Sub FrontendMsg
subscriptions _ =
    receiveSelection GotSelection


content : Model -> Element msg
content model =
  el
        [ htmlAttribute (HtmlAttribute.style "white-space" "normal")
        ]
    <| Element.html
    <| Markdown.toHtmlWith myOptions [] model.text

myOptions : Markdown.Options
myOptions =
    { githubFlavored = Just { tables = True, breaks = False }
    , defaultHighlighting = Nothing
    , sanitize = True
    , smartypants = False
    }

testText : String
testText = """
### **Apuntes: Algoritmos**

#### **1. ¿Qué es un algoritmo?**
Un **algoritmo** es un conjunto de pasos organizados y finitos que se deben seguir para resolver un problema o realizar una tarea específica.  
Es como una receta de cocina: contiene instrucciones claras que, al ejecutarse, producen un resultado esperado.

**Ejemplo sencillo:**  
- Problema: Quieres calcular la suma de dos números.  
- Algoritmo:  
  1. Toma el primer número.  
  2. Toma el segundo número.  
  3. Suma ambos números.  
  4. Muestra el resultado.  

---

#### **2. Características principales de un algoritmo**
Para que algo sea considerado un algoritmo, debe cumplir con estas características:  
1. **Definido:** Cada paso debe ser claro y no debe generar ambigüedad.  
   - Ejemplo: "Suma los números" es claro, mientras que "Haz algo con los números" es ambiguo.  

2. **Finito:** Debe tener un inicio y un final; no puede ejecutarse indefinidamente.  
   - Ejemplo válido: "Calcula la suma de dos números y muestra el resultado."  
   - Ejemplo no válido: "Sigue sumando números para siempre."  

3. **Entrada y salida:** Debe recibir datos de entrada y producir un resultado (salida).  
   - Entrada: Dos números, por ejemplo, 3 y 5.  
   - Salida: El resultado de la suma, que es 8.  

4. **Efectividad:** Los pasos deben ser realizables con los recursos disponibles.  

---

#### **3. Representación de algoritmos**
Un algoritmo puede representarse de varias formas. Las más comunes son:  

##### **3.1 Pseudocódigo**  
Es una forma de describir un algoritmo utilizando texto estructurado que se asemeja a un lenguaje de programación, pero es fácil de leer para humanos.  

**Ejemplo de pseudocódigo (calcular el área de un triángulo):**  
```
Inicio
  Leer base
  Leer altura
  Calcular área ← (base * altura) / 2
  Mostrar área
Fin
```

##### **3.2 Diagramas de flujo**  
Son diagramas visuales que representan los pasos de un algoritmo mediante símbolos gráficos.  

**Símbolos básicos en diagramas de flujo:**  
- **Óvalo:** Inicio/Fin del algoritmo.  
- **Rectángulo:** Procesos (cálculos, acciones).  
- **Rombo:** Decisiones (condiciones, bifurcaciones).  
- **Flechas:** Indican el flujo de ejecución.

---

#### **4. Tipos de algoritmos**
Dependiendo de su propósito, los algoritmos pueden clasificarse en diferentes tipos:  

1. **Algoritmos secuenciales:**  
   - Los pasos se ejecutan en orden, uno después del otro.  
   - Ejemplo: Calcular el promedio de tres números.  

2. **Algoritmos condicionales:**  
   - Incluyen decisiones que dependen de una condición (por ejemplo, "si ocurre X, haz Y").  
   - Ejemplo:  
     ```
     Si la temperatura es mayor a 30°C
        Mostrar "Hace calor"
     Sino
        Mostrar "Hace frío"
     ```

3. **Algoritmos iterativos:**  
   - Repiten uno o varios pasos hasta que se cumpla una condición.  
   - Ejemplo: Imprimir los números del 1 al 10.  

---

#### **5. Ejemplo práctico completo**
**Problema:** Diseña un algoritmo para determinar si un número es par o impar.  

##### **Solución en pseudocódigo:**  
```
Inicio
  Leer número
  Si (número mod 2 = 0) Entonces
    Mostrar "El número es par"
  Sino
    Mostrar "El número es impar"
Fin
```

##### **Explicación del ejemplo:**  
- El paso clave es calcular el módulo del número con 2 (número mod 2).  
- Si el resultado es 0, significa que el número es divisible entre 2 (es par).  
- En caso contrario, es impar.  

---

#### **6. Aplicaciones de los algoritmos**
Los algoritmos son la base de la informática y tienen aplicaciones en numerosos campos, como:  

1. **Búsqueda:** Encontrar información en una base de datos o archivo (ej.: Algoritmo de búsqueda binaria).  
2. **Ordenamiento:** Organizar datos (ej.: Algoritmo de burbuja, quicksort).  
3. **Cifrado y seguridad:** Proteger información mediante algoritmos de encriptación (ej.: RSA, AES).  
4. **Inteligencia artificial:** Resolver problemas complejos, como la clasificación de imágenes o el reconocimiento de voz.  

---

#### **7. Buenas prácticas al diseñar algoritmos**
1. **Divide y vencerás:** Divide el problema en partes más pequeñas y resuélvelas individualmente.  
2. **Prueba y error:** Ejecuta el algoritmo con diferentes datos para verificar su funcionamiento.  
3. **Optimización:** Busca que el algoritmo sea lo más eficiente posible (menos pasos o menos recursos).  

---

### **¿Cómo puedes interactuar con estos apuntes?**
- Si hay algo que no entiendas, selecciona la parte específica y elige la opción "No entiendo".  
- Si encuentras útil algún ejemplo, selecciona "Me gusta".  
- Si algo no te convence o consideras que falta información, selecciona "No me gusta".  

"""