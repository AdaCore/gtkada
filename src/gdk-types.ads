with Glib; use Glib;

package Gdk.Types is

   type Gdk_Cap_Style is (Not_Last, Butt, Round, Projecting);

   type Gdk_Fill is (Solid, Tiled, Stippled, Opaque_Stippled);

   type Gdk_Fill_Rule is (Even_Odd_Rule, Winding_Rule);

   type Gdk_Function is (Copy, Invert, Gdk_Xor);

   type Gdk_Join_Style is (Miter, Round, Bevel);

   type Gdk_Line_Style is (Solid, On_Off_Dash, Double_Dash);

   type Gdk_Overlap_Type is (Rectangle_In, Rectangle_Out, Rectangle_Part);

   type Gdk_Subwindow_Mode is (Clip_By_Children,
                               Include_Inferiors);

   type Gdk_Values_Mask is (Foreground,
                            Background,
                            Font,
                            GC_Function,
                            Fill,
                            Tile,
                            Stipple,
                            Clip_Mask,
                            Subwindow,
                            Ts_X_Origin,
                            Tx_Y_Origin,
                            Clip_X_Origin,
                            Clip_Y_Origin,
                            Exposures,
                            Line_Width,
                            Line_Style,
                            Cap_Style,
                            Join_Style);


   -------------------
   --  Rep clauses  --
   -------------------

   for Gdk_Subwindow_Mode use (Clip_By_Children => 0,
                               Include_Inferiors => 1);

   for Gdk_Values_Mask use (Foreground    => 2 ** 0,
                            Background    => 2 ** 1,
                            Font          => 2 ** 2,
                            GC_Function   => 2 ** 3,
                            Fill          => 2 ** 4,
                            Tile          => 2 ** 5,
                            Stipple       => 2 ** 6,
                            Clip_Mask     => 2 ** 7,
                            Subwindow     => 2 ** 8,
                            Ts_X_Origin   => 2 ** 9,
                            Tx_Y_Origin   => 2 ** 10,
                            Clip_X_Origin => 2 ** 11,
                            Clip_Y_Origin => 2 ** 12,
                            Exposures     => 2 ** 13,
                            Line_Width    => 2 ** 14,
                            Line_Style    => 2 ** 15,
                            Cap_Style     => 2 ** 16,
                            Join_Style    => 2 ** 17);

end Gdk.Types;
