module Graphics.UI.Gtk.WebKit.DOM.DOMTokenList
       (domTokenListItem, domTokenListContains, domTokenListAdd,
        domTokenListRemove, domTokenListToggle, domTokenListGetLength,
        DOMTokenList, DOMTokenListClass, castToDOMTokenList,
        gTypeDOMTokenList, toDOMTokenList)
       where
import System.Glib.FFI
import System.Glib.UTFString
import Control.Applicative
{#import Graphics.UI.Gtk.WebKit.Types#}
import System.Glib.GError
import Graphics.UI.Gtk.WebKit.DOM.EventM

domTokenListItem ::
                 (DOMTokenListClass self, GlibString string) =>
                   self -> Word -> IO string
domTokenListItem self index
  = ({# call webkit_dom_dom_token_list_item #} (toDOMTokenList self)
       (fromIntegral index))
      >>=
      readUTFString

domTokenListContains ::
                     (DOMTokenListClass self, GlibString string) =>
                       self -> string -> IO Bool
domTokenListContains self token
  = toBool <$>
      (propagateGError $
         \ errorPtr_ ->
           withUTFString token $
             \ tokenPtr ->
               {# call webkit_dom_dom_token_list_contains #} (toDOMTokenList self)
                 tokenPtr
             errorPtr_)

domTokenListAdd ::
                (DOMTokenListClass self, GlibString string) => self -> string -> IO ()
domTokenListAdd self token
  = propagateGError $
      \ errorPtr_ ->
        withUTFString token $
          \ tokenPtr ->
            {# call webkit_dom_dom_token_list_add #} (toDOMTokenList self)
              tokenPtr
          errorPtr_

domTokenListRemove ::
                   (DOMTokenListClass self, GlibString string) => self -> string -> IO ()
domTokenListRemove self token
  = propagateGError $
      \ errorPtr_ ->
        withUTFString token $
          \ tokenPtr ->
            {# call webkit_dom_dom_token_list_remove #} (toDOMTokenList self)
              tokenPtr
          errorPtr_

#if WEBKIT_CHECK_VERSION(2,0,0)
domTokenListToggle ::
                   (DOMTokenListClass self, GlibString string) =>
                     self -> string -> Bool -> IO Bool
domTokenListToggle self token force = toBool <$> (propagateGError $ \errorPtr_ -> withUTFString token $ \tokenPtr ->
    {# call webkit_dom_dom_token_list_toggle #} (toDOMTokenList self) tokenPtr (fromBool force) errorPtr_)
#else
domTokenListToggle ::
                   (DOMTokenListClass self, GlibString string) => self -> string -> IO Bool
domTokenListToggle self token
  = toBool <$>
      (propagateGError $
         \ errorPtr_ ->
           withUTFString token $
             \ tokenPtr ->
               {# call webkit_dom_dom_token_list_toggle #} (toDOMTokenList self)
                 tokenPtr
             errorPtr_)
#endif

domTokenListGetLength ::
                      (DOMTokenListClass self) => self -> IO Word
domTokenListGetLength self
  = fromIntegral <$>
      ({# call webkit_dom_dom_token_list_get_length #}
         (toDOMTokenList self))
