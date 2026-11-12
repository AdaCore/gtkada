--  Application callback types for gtk-expression.required-watch-callback-nullable.
--  They deliberately allow null, unlike GTK's required Notify parameter.
--  Library-level declarations give them the same accessibility as GtkAda's
--  callback types, so normal Ada access-type conversions are legal.
package Optional_Callbacks is
   type Notify is access procedure;
   type Notify_With_Data is access procedure (User_Data : Integer);
end Optional_Callbacks;
