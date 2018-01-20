
# PureScript Universal Web App

A model of a client-server HTML rendering system.

## Motivation

Current PureScript web frameworks offer solutions to the problem of having data in a browser window and rendering to the DOM, either by overwriting an existing DOM node (primitive DOM APIs) or updating an existing DOM node it previously updated (VirtualDOM, SimpleDOM). These frameworks don't offer answers to the complexities of writing large, multi-page website apps. Such complexities include:

- How to request just one URL without loading the templates required for all URLs (SPA vs website)
- How to request a URL and get just the HTML, rather than the JavaScript used to render that HTML (server-side rendering)
- What state to keep between pages (redux store)

Complexities of nice to have features:

- How to improve speed of loading common URLs (pre-fetch commonly used linked URLs)


## Website Architecture

The web traditionally works by requesting an HTML page with an HTTP request. That's it - very simple.


### Handling an HTTP Request

The web traditionally works by requesting an HTML page with an HTTP request.

``` purescript
newtype HTML = HTML String
newtype HTTPRequest i o = HTTPRequest
  { method :: HTTPMethod
  , uri :: i -> URI
  , acceptHeader :: o -> AcceptType
  , otherHeaders :: List Header
  , body :: i -> Maybe String
  }
```

Web browsers will never refuse to render an incorrectly-formatted HTML page, instead they will do their best to render it as it was intended to be, so `newtype HTML = HTML String` is an accurate description. Despite that, we want to ensure the browser doesn't incorrectly interpret our intent, so we should verify the HTML formatting of an `HTML` value in the build pipeline using an HTML linter program and/or a produce it using a type-checked language like PureScript. We can do that elsewhere.

Rendering the HTML string usually requires getting data from a database, so the HTML should be the result of an effect. It's an asynchronous thing in Node, so the HTML will need to be returned in a continuation. Also, the function could fail, so we'll need to return it in an `Either` or `ExceptT`.

``` purescript
MonadEff m => HTTPRequest i o -> ContT Unit (ExceptT err m) o
type HTTPRequestHandlerM m err o = ContT Unit (ExceptT err m) o
data HTTPRequestHandler m err i o = HTTPRequestHandler (HTTPRequest i o -> RequestHandlerM err effs o)
```

An example `HTTPRequestHandler`: Mustache template

``` purescript
mustacheHTTPRequestHandler :: forall err effs. HTTPRequestHandler err effs
mustacheHTTPRequestHandler = HTTPRequestHandler mustacheHandler
  where
    mustacheHandler :: HTTPRequest -> RequestHandlerM err effs
    mustacheHandler req = fetchDataForSomeMustacheTemplate req >>= renderMustacheTemplate
    fetchDataForSomeMustacheTemplate :: HTTPRequest -> RequestHandlerM SomeMustacheData
    fetchDataForSomeMustacheTemplate = ...
    renderMustacheTemplate :: SomeMustacheData -> RequestHandlerM HTML
    renderMustacheTemplate data = HTML <<< Mustache.render <<< someMustacheTemplate $ data
```


### Handling an HTML Request

A browser can send an HTTP request to a server, but a server can *not* send an HTTP request to a browser. A server *can* send data to a browser using one of a few strategies, such as HTTP long-polling, HTML server-sent events, or TCP websockets, but these are out-of-scope of this article, which is about modelling a client-server HTML rendering system. So, a browser's HTML page request can use a higher-level of abstraction than an HTTP request.

Because an in-browser app can't fulfill an HTTP request, we need a different concept. From a browser user's point of view, an HTML page is requested. If we use the concept of an HTML request handler, both a server *and a browser* can handle it, as a browser provides multiple ways of intercepting an HTML request, such as anchor element's on-click handler or a service worker.

``` purescript
data HTMLRequest = HTMLRequest
  HierarchicalPart -- www.google.com/some/path
  (Maybe Query)
  (Maybe Fragment)

htmlRequestToHTTPRequest :: HTMLRequest -> HTTPRequest Unit HTML
htmlRequestToHTTPRequest (HTMLRequest hostPath query fragment) =
  HTTPRequest
    { method: GET
    , uri: URI Nothing hostPath query fragment
    , acceptHeader: const ApplicationHTML
    , otherHeaders: mempty
    , body: const Nothing
    }

requestHTML' :: Maybe Authority -> URIPathAbs -> Maybe Query -> Maybe Fragment -> HTMLRequest
requestHTML' authority path query fragment =
  HTTPRequest
    { method: GET
    , uri: URI Nothing (HierarchicalPart authority path) query fragment
    , acceptHeader: const ApplicationHTML
    , otherHeaders: mempty
    , body: const Nothing
    }

-- More commonly used like this
-- getOurHTML :: URIPathAbs -> Maybe Query -> HTMLRequest
-- getOurHTML path query = requestHTML someAuthority path query Nothing
```

Now that we have an `HTMLRequest`, we can define a program which handles that request.

``` purescript
-- type HTMLRequestHandlerM m err = ContT Unit (ExceptT err m) HTML
type HTMLRequestHandlerM m = ContT Unit m HTML
data HTMLRequestHandler m = HTMLRequestHandler (HTMLRequest -> HTMLRequestHandlerM m)
```

An example `HTMLRequestHandler`:

``` purescript
browserHTMLRequestHandler :: HTMLRequestHandler m err
browserHTMLRequestHandler = HTMLRequestHandler h
  where
    h :: HTMLRequest -> HTMLRequestHandlerM m
    h htmlReq = do
      oneOfMyRoutes <- matchRoute htmlReq
      fromMaybe (handleUnknownRouteRequest htmlReq)
        (fetchMyRouteHTML <$> oneOfMyRoutes)
    matchRoute :: HTMLRequest -> m (Maybe MyRoute)
    matchRoute = ...
    fetchMyRouteHTML :: MyRoute -> m HTML
    fetchMyRouteHTML = ...
    handleUnknownRouteRequest :: HTMLRequest -> m HTML
    handleUnknownRouteRequest req =
      if isOtherAuthority req
        then redirectTo req
        else redirectTo myHomePage
```


### Dynamic HTML

Modern websites use custom DOM elements such as:

- Pure UI, such as a drop-down list or menu
- Behavior element, such as a twisty control which expands/collapses a list
- Effectful element, such as submitting a form
- Remote data CRUD, such as a data table and its API requests
- State container, such as list-detail bi-view onto same data store

We call these "dynamic HTML components", because the DOM created by this HTML is dynamic - it updates in response user interaction, the time of day, or signals emitted by neighboring components. This dynamic behavior is implemented in JavaScript, listening for these events and appropriately updating the DOM. For example, when the user interacts with a twisty element, JavaScript function responds to this event to change another element's CSS value or removes an element from the DOM.

Notes:
- An HTML page as a whole entity is not dynamic, but components in its body are. We need to define these two things separately, then, such that a `HTML` is dynamic, not `HTMLPage`, the page in which it's mounted.
- These elements can become quite complex and managing the complexity can be quite difficult. A helpful tool to mitigate this problem is a type-checker which checks references between HTML and JavaScript. Such a tool is implemented by parsing an `HTML` string or by writing both the HTML and its supporting JavaScript in the same language.
- There is a variety of ways to make an `HTML` value using PureScript -- VirtualDOM, StaticDOM, Mustache templates -- so we can't presume a certain tool was used to make it.

For the purposes of this project, a client-server HTML rendering system, a dynamic HTML component needs the following attributes:
- Two rendering targets - an HTML file for a browser-requested page, and a DOM creator/hydrator for an in-browser app patching an existing `HTMLDocument`.
- An asynchronous renderer and asynchronous event handlers - e.g. waiting on a database response before rendering the HTML or waiting for an API response when clicking on a closed twisty.
- A data serializer - data retrieved when server-side rendering a page can be serialized, sent to browser with its hydration code, and given to it when it's instantiated.

``` purescript
-- A dynamic component needs supporting JavaScript
newtype JavaScript = JavaScript String

-- ??? How much of these should the client-server HTML rendering platform prescribe?
-- A dynamic HTML component attaches behavior before mounting at a certain lifecycle point.
-- Instantiation process:
--  - if isSSRed target then claim targetNodes else create nodes
--      "claim" and "create" add listeners and attributes (need to do attributes???)
--  - detach unneededNodes
--  - mount nodes on targetNodes
newtype DynamicHTMLJavaScript = DynamicHTMLJavaScript
  -- \/ Initialize nodes used in component and keep in component's scope.
  { create :: JavaScript
  -- \/ Given children of target node, keep refs matching desired state in component's scope.
  --      Any unmatched nodes should be detached from DOM.
  , claim :: JavaScript -- (Array Node) -> Eff _ (Array Node)
  -- \/ Mount nodes in component's scope to DOM at target node.
  , mount :: JavaScript
  -- \/ Attach listeners and perhaps data-dependent attributes.
  , hydrate :: JavaScript
  -- \/ Detach component's nodes from DOM, but keeps managed nodes instantiated.
  , unmount :: JavaScript -- ?? Rename to "destroy"?
  -- \/ Destroy component's state and scope, remove listeners. Should have already unmounted.
  , unmount :: JavaScript -- ?? Rename to "destroy"?
  }

-- Any HTML having hydrating JavaScript is DynamicHTML
-- DynamicHTML is not composable, it's a box used to move a dynamic HTML component from
--   server to client and to return a form appropriate to the requestor - either the browser or in-browser app.
-- ??? It's ok this isn't composable? If composable, how to compose hydration scripts?
--data DynamicHTML a = DynamicHTML HTML JavaScript (Maybe a)
data DynamicHTML a = DynamicHTML HTML DynamicHTMLJavaScript (Maybe a)

-- Anything rendering both HTML and its hydrating JavaScript is a DynamicHTMLProgram
-- ??? Define elsewhere? Not always puts code in script tag.
data DynamicHTMLProgram a = DynamicHTMLProgram
  -- on server
  { renderAsHTMLString :: (a -> HTML)
  , renderSupportingJavaScript :: (a -> JavaScript)
  -- on browser
  , mount :: (forall m. JavaScript -> DOMElement -> m Unit)
  , hydrateDOM :: (forall m. JavaScript -> DOMElement -> m Unit)
  }
runDynamicHTMLProgram :: DynamicHTMLProgram a -> a -> DynamicHTML
runDynamicHTMLProgram (DynamicHTMLProgram renderHTML renderJS hydrate) program =
  DynamicHTML (renderHTML program) (renderJS program) data
```

Let's test this model on a real-world example - a React-managed dynamic HTML element.

``` purescript
reactHTMLProgram :: DynamicHTMLProgram ReactElement
reactHTMLProgram = DynamicHTMLProgram
  { renderAsHTMLString: ReactDOMServer.renderToString
  -- \/ the whole ReactComponent is the supporting JS
  , renderSupportingJavaScript: JavaScript <<< unsafeCoerce
  , mount: ReactDOM.render
  , hydrateDOM: ReactDOM.hydrate
  }

hello :: ReactElement
hello = D.div' [ D.text "Hello" ]

httpResponse :: DynamicHTML
httpResponse = runDynamicHTMLProgram reactHTMLProgram hello

handleHTMLRequest :: HTMLRequest -> Eff Unit
handleHTMLRequest req = respond html
  where
    matchedPage :: ReactElement
    dynHtml :: DynamicHTML
    dynHtml = runDynamicHTMLProgram reactHTMLProgram hello
    html :: DynamicHTML -> HTML
    html (DynamicHTML h js d) =
      "<html><head></head><body>"
      <> h
      <> "<script>"
      <> js
      <> "document.onload(function() {"
      <> js.onload
      <> "});"
      <> "</script>"
      <> "</body></html>"

handleInBrowserHTMLRequest :: HTMLRequest -> HTMLRequestHandler m
handleInBrowserHTMLRequest req =
  loadRoute app
  where
    routeFromHtmlReq :: HTMLRequest -> Route
    loadRoute :: Route -> ContT Unit _ DynamicHTML -- in-browser lazy-load this module
    htmlGen :: DynamicHTML

-- !!! Need JS for create, not only hydrate
mountInBrowserHTML :: DynamicHTML -> Eff _ Unit
mountInBrowserHTML (DynamicHTML h js d) = exec js
  where exec :: JavaScript -> Eff _ Unit
```


### Dynamic HTML Components

!!! What would `DynamicHTMLComponent` look like? How would that compose?
??? Concept of DynamicHTMLComponentRoot or DynamicHTMLComponentManager,
      which is a single component/template and manages its children in a single scope,
      for efficiency.
Leave undefined until we attempt a POC of this system.


#### Dynamic HTML Components: Unifying Client-server Execution Environments

A dynamic HTML component will need to use features of the runtime environment which have different APIs in the browser and server, such as HTTP requests. Rather than choosing one runtime system's API and re-implementing it in the other, we can program our components to use a common interface/request format and have the runtime system execute the requested effects in the best manner for that specific runtime system.

``` purescript
-- ??? HTMLRequestHandler isn't a component's execution environment...
--       Perhaps 
type HTMLRequestHandlerM m = ContT Unit m HTML
data HTMLRequestHandler m = HTMLRequestHandler (HTMLRequest -> HTMLRequestHandlerM m)
type HTMLRequestHandlerRun eff = HTMLRequestHandler (Run eff)
```

#### Who Controls Head Content?

??? This seems like responsibility of HTMLRequestHandler, as it returns an HTML page.
  But actually, HTMLRequestHandler just returns an unstructured HTML String,
  which means that it's the responsibility of whatever creates that HTML page.
  This doesn't agree with how the in-browser app's HTMLRequestHandler works, which
  only receives part of an HTML page.

The content of `Head` comes from several places:
- `HTTPRequestHandler`, as it knows the `HTTPRequest` and composed the `HTMLPage`, like language, character set, and author and open-graph metadata
- `HTMLComponent` in the page, as it has specific data which can be used to populate metadata, such as a blog post title, and may want to set the page title

As a first opinion, it seems best to offer no special way for an HTMLComponent to set a page's title, and instead have it use DOM APIs to set the page title.

``` purescript
-- Maybe not necessary, but when we start 
newtype Head = Head String
newtype Body = Body String
newtype Attributes = Attributes (StrMap String)
data HTMLPage = HTMLPage Attributes Head Body
renderHTMLPage :: HTMLPage -> HTML
renderHTMLPage (HTMLPage attrs h b) = writeHTML attrs (head j <> body b)
  where
    head h = "<head>" <> unwrap h <> "</head>"
    body h = "<body>" <> unwrap h <> "</body>"
    writeHTML :: Attributes -> HTML -> HTML
    writeHTML a h = renderAttributes a <> h
```


### Nav


-- !!! Start working from here.


### HTML Components ??? Move this to way later?

If `HTMLProgram` only makes a *section* of a page, then its arguments will be different. For example, a date-picker's input might be the dates to show as unavailable: `List Date -> HTML`. In this case, it isn't making a whole page, but only a component of the page, so it would actually be producing something different: `List Date -> HTMLElementString`.

``` purescript
newtype HTMLElementProgram m i = HTMLElementProgram (i -> m HTMLElementString)
HTMLElementProgram m i -> m HTMLElementString
runHTMLElementProgram :: HTMLElementProgram m i -> i -> m HTMLElementString
```


### Continuing an `HTMLProgram` in the Browser

After an `HTMLProgram` generates an `HTML` on the server, it is sent to the browser. Modern HTML pages need JavaScript to support the HTML, such as a date-picker component in a form. How does this JavaScript get sent in the response? JavaScript is included in an HTML page using a `<script>` element. An `HTML` value is just a String, which is hard to manipulate, so we'll expect that it is whatever produced that `HTML` value's responsibility.

If not `HTML`'s responsibility, then including an `HTMLElement`'s supporting JavaScript must be the responsibility of whatever produced that `HTML` value, which is the `HTMLProgram`. Therefore, to make an `HTML` value which includes the supporting JavaScript will require a custom `HTMLProgram`, one which works with a more structured definition of HTML, which we will call `HTMLPage`.

``` purescript
data HTMLPage = HTMLPage HTMLElement Script CSS
-- data HTMLPage = HTMLPage view behavior style
-- data HTMLPage = HTMLPage HTMLHead HTMLBody Script CSS -- might want this instead
newtype HTMLPageProgram m = HTMLPageProgram (HTTPRequest -> m HTMLPage)
renderHTMLPageProgram :: HTMLPageProgram m -> (HTMLPage -> m HTML) -> HTMLProgram m
renderHTMLPageProgram (HTMLPageProgram f) render = HTMLProgram (render =<< f)
```


#### Refining the HTMLPage Type

How `HTMLElement` and `Script` interact is something we should consider. Also, if `Script` needs access to browser APIs it will need an `m` variable for the HKT it's in. The definitions of these seems fuzzy, so let's consider how they should be defined.

```

```
