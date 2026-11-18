--  Application callback type for gtk-builder_cscope.callback-symbol-nullability.
--  It deliberately allows null, unlike GTK's required callback symbol. Its
--  library-level declaration and Ada convention match G_Callback's, so that
--  converting it to G_Callback is legal Ada.
package Optional_Callbacks is
   type Callback is access procedure;
end Optional_Callbacks;
