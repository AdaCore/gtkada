with Ada.Text_IO;       use Ada.Text_IO;
with Gdk.Pixbuf;
with Gdk.Pixbuf_Format; use Gdk.Pixbuf_Format;
with GNAT.Strings;

procedure Pixbuf_Formats is
   use type Format_List.GSlist;
   use type GNAT.Strings.String_Access;

   procedure Put_Values
     (Label : String; Values : in out GNAT.Strings.String_List);
   --  Print and release the strings returned by the format getters.

   procedure Put_Values
     (Label : String; Values : in out GNAT.Strings.String_List) is
   begin
      Put (Label);
      for Index in Values'Range loop
         if Index /= Values'First then
            Put (", ");
         end if;
         if Values (Index) /= null then
            Put (Values (Index).all);
         end if;
         GNAT.Strings.Free (Values (Index));
      end loop;
      New_Line;
   end Put_Values;

   Formats : Format_List.GSlist := Gdk.Pixbuf.Get_Formats;
   Cursor  : Format_List.GSlist := Formats;
begin
   if Formats = Format_List.Null_List then
      Put_Line ("No GdkPixbuf formats are available.");
   end if;

   while Cursor /= Format_List.Null_List loop
      declare
         Format : constant Gdk_Pixbuf_Format_Access :=
           Format_List.Get_Data (Cursor);
         Extensions : GNAT.Strings.String_List := Get_Extensions (Format);
         Mime_Types : GNAT.Strings.String_List := Get_Mime_Types (Format);
      begin
         Put_Line (Get_Name (Format) & " - " & Get_Description (Format));
         Put_Values ("  Extensions: ", Extensions);
         Put_Values ("  MIME types: ", Mime_Types);
         Put_Line
           ("  Read: " & (if Is_Disabled (Format) then "disabled" else "yes")
            & "; write: " & (if Is_Writable (Format) then "yes" else "no"));
         New_Line;
      end;
      Cursor := Format_List.Next (Cursor);
   end loop;

   --  GdkPixbuf owns the formats; only the list nodes belong to the caller.
   Format_List.Free (Formats);
end Pixbuf_Formats;
