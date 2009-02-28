-------------------------------------------------------------------------------
--                                                                           --
-- acolor -- Ada/ANSI color                                                  --
-- A simple program to ease ANSE colored text output from your shell script. --
--                                                                           --
-- Copyright (c) 2009, Zhu Qun-Ying. All Rights Reserved.                    --
--                                                                           --
-- This program is free software; you can redistribute it and/or modify      --
-- it under the terms of the GNU General Public License as published by      --
-- the Free Software Foundation; either version 3 of the License, or         --
-- (at your option) any later version.                                       --
--                                                                           --
-- This program is distributed in the hope that it will be useful,           --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of            --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             --
-- GNU General Public License for more details.                              --
--                                                                           --
-- You should have received a copy of the GNU General Public License         --
-- along with this program; if not, see <http://www.gnu.org/licenses/>.      --
-------------------------------------------------------------------------------


with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Characters;           use Ada.Characters;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Characters.Latin_1;

procedure acolor is
   type color is (black, red, green, yellow, blue, magenta, cyan, white);
   type attribute is (nm, normal, bd, bold, ft, faint, it, italic,
		      ul, underline, bl, blink, fb, fastblink, rv, reversed,
		      iv, invisible);
   type attrs is array (attribute'Range) of Boolean;
   pragma Pack (attrs);

   type color_setting is
   record
      fg_color  : color;
      bg_color  : color;
      attr      : attrs;
      fg_set    : Boolean;
      bg_set    : Boolean;
   end record;

   ---------------------------------------------------------------------------
   version   : constant String := "1.1.1 20090227";
   -- use stream output to avoid the terminal CR by text_io
   std_out   : Stream_Access;
   in_setting : color_setting;

   ---------------------------------------------------------------------------
   procedure put_string (code : in String) is
   begin
      String'Write (std_out, code);
   end put_string;
   pragma Inline (put_string);

   ---------------------------------------------------------------------------
   procedure put_reset is
   begin
      put_string (Latin_1.ESC & "[0m");
   end put_reset;

   ---------------------------------------------------------------------------
   procedure put_escape_sequence (in_setting : in color_setting) is
      fg_color  : constant array (color'Range) of String (1 .. 2) :=
	("30", "31", "32", "33", "34", "35", "36", "37");
      bg_color  : constant array (color'Range) of String (1 .. 2) :=
	("40", "41", "42", "43", "44", "45", "46", "47");
      first_attr_set : Boolean := False;
      i : attribute;
   begin
      put_string (Latin_1.ESC & '[');
      i := attribute'First;
      loop
	 if in_setting.attr (i)
	   or else in_setting.attr (attribute'Succ (i)) then
	    if not first_attr_set then
	       case i is
		  when nm | normal     => put_string ("0");
		  when bd | bold       => put_string ("1");
		  when ft | faint      => put_string ("2");
		  when it | italic     => put_string ("3");
		  when ul | underline  => put_string ("4");
		  when bl | blink      => put_string ("5");
		  when fb | fastblink  => put_string ("6");
		  when rv | reversed   => put_string ("7");
		  when iv | invisible  => put_string ("8");
	       end case;
	       first_attr_set := True;
	    else
	       case i is
		  when nm | normal     => put_string (";0");
		  when bd | bold       => put_string (";1");
		  when ft | faint      => put_string (";2");
		  when it | italic     => put_string (";3");
		  when ul | underline  => put_string (";4");
		  when bl | blink      => put_string (";5");
		  when fb | fastblink  => put_string (";6");
		  when rv | reversed   => put_string (";7");
		  when iv | invisible  => put_string (";8");
	       end case;
	    end if;
	 end if;
	 exit when i = attribute'Pred (attribute'Last);
	 i := attribute'Succ (attribute'Succ (i));
      end loop;

      if in_setting.fg_set then
	 if first_attr_set then
	    put_string (";");
	 end if;
	 put_string (fg_color (in_setting.fg_color));
	 if in_setting.bg_set then
	    put_string (";" & bg_color (in_setting.bg_color));
	 end if;
      end if;
      put_string ("m");
   end put_escape_sequence;

   procedure put_one_color_line (bg_color : in color;
				 attr	  : in attrs;
				 prefix	  : in String) is
      setting : color_setting;
   begin
      put_string (" ");
      setting.fg_set   := True;
      setting.bg_set   := True;
      setting.attr     := attr;
      setting.bg_color := bg_color;
      for i in color'Range loop
	 setting.fg_color := i;
	 put_escape_sequence (setting);
	 put_string (prefix & To_Lower (color'Image (i)) & " ");
	 put_reset;
	 put_string (" ");
      end loop;
      New_Line;
   end put_one_color_line;
   ---------------------------------------------------------------------------
   procedure put_color_list is
   begin
      for i in color'Range loop
	 put_one_color_line (i, attrs'(nm => True, others => False), "   ");
	 put_one_color_line (i, attrs'(bd => True, others => False), " lt");
      end loop;
   end put_color_list;

   ---------------------------------------------------------------------------
   procedure color_put (str	 : in String;
			fg_color : in color := white;
			bg_color : in color := black;
			attr	 : in attrs := (normal => True,
					     	others => False)) is
      setting : constant color_setting :=
        (fg_color => fg_color,
	 fg_set	  => True,
	 bg_color => bg_color,
	 bg_set	  => True,
	 attr	  => attr);
   begin
      put_escape_sequence (setting);
      put_string (str);
      put_reset;
   end color_put;
   ---------------------------------------------------------------------------
   procedure bd_color_put (str    : in String;
			   fg_color : in color := white;
			   bg_color : in color := black) is
      attr : constant attrs := (bold => True, others => False);
   begin
      color_put (str, fg_color, bg_color, attr);
   end bd_color_put;
   pragma Inline (bd_color_put);
   ---------------------------------------------------------------------------
   procedure color_putl (str    : in String;
			 fg_color : in color := white;
			 bg_color : in color := black;
			 attr	  : in attrs := (normal => True,
						 others => False)) is
   begin
      color_put (str, fg_color, bg_color, attr);
      New_Line;
   end color_putl;
   pragma Inline (color_putl);
   ---------------------------------------------------------------------------
   procedure print_usage is
   begin
      bd_color_put ("a", red);
      bd_color_put ("c", green);
      bd_color_put ("o", yellow);
      bd_color_put ("l", blue);
      bd_color_put ("o", magenta);
      bd_color_put ("r", cyan);

      Put (" " & version);
      bd_color_put (" Copyright (C) 2009, Zhu Qun-Ying", green);
      New_Line;
      color_putl ("This program is free software released under the GPLv3 " &
	          "or latter.", yellow);
      bd_color_put ("Usage:", green);
      color_put (" acolor", cyan);
      put_string (" [ ");
      color_put ("effect", magenta);
      put_string (" ] [ [lt]");
      color_put ("fgcolor", green);
      put_string (" ] [ ");
      color_put ("bgcolor", green);
      Put_Line (" ]");

      Put_Line  ("              " &
	         "off        turns off any coloring and resets " &
	         "to default colors.");
      Put_Line  ("              " &
	         "-l,--list  shows all the possible color " &
	         "combinations visually.");
      Put_Line  ("              " &
	         "-h,--help  list this help");
      New_Line;
      put_string ("  ");
      color_put ("effect", magenta);
      put_string (" could be multiple of (some ");
      color_put ("effect", magenta);
      Put_Line ("s may not work for your terminal):");
      put_string ("    ");
      color_put ("bd bold", white, black,
	         attrs'(bold => True, others => False));
      put_string (" ");
      color_put ("nm normal", white, black);
      put_string (" ");
      color_put ("ft faint", white, black,
	         attrs'(faint => True, others => False));
      put_string (" ");
      color_put ("it italic", white, black,
	         attrs'(italic => True, others => False));
      put_string (" ");
      color_put ("ul underline", white, black,
	         attrs'(underline => True, others => False));
      put_string (" ");
      color_put ("bl blink", white, black,
	         attrs'(blink => True, others => False));
      put_string (" ");
      color_putl ("fb fastblink", white, black,
		  attrs'(fastblink => True, others => False));
      put_string ("    ");
      color_put ("rv reversed", white, black,
	         attrs'(reversed => True, others => False));
      put_string (" ");
      color_put ("iv invisible");
      New_Line;

      put_string ("  ");
      color_put ("fg_color", green);
      put_string (" and ");
      color_put ("bg_color", green);
      Put_Line (" are one of:");
      put_string ("    ");
      color_put ("black",     black, white);
      color_put (" red",      red);
      color_put (" green",    green);
      color_put (" yellow",   yellow);
      color_put (" blue",     blue);
      color_put (" magenta",  magenta);
      color_put (" cyan",     cyan);
      color_putl (" white",    white);
      Put ("  In addition, ");
      color_put ("fg_color", green);
      Put_Line (" could also be one of:");
      put_string ("    ");
      bd_color_put ("ltblack",     black);
      bd_color_put (" ltred",      red);
      bd_color_put (" ltgreen",    green);
      bd_color_put (" ltyellow",   yellow);
      bd_color_put (" ltblue",     blue);
      bd_color_put (" ltmagenta",  magenta);
      bd_color_put (" ltcyan",     cyan);
      bd_color_put (" ltwhite",    white);
      New_Line (2);
      Put ("  It is compatible with Moshe Jacobson's color program. " &
	        "Soft link it to ");
      color_putl ("color", cyan);
      Put_Line ("  to avoid rewrite your script.");
      bd_color_put ("Example:", green);
      Put_Line (" In a POSIX compliant shell such as bash or ksh, " &
	        "you would do:");

      put_string ("    ");
      color_putl ("echo ""$(acolor ul yellow blue rv)Hi there.$(acolor off)""",
	          black, white);
      put_string ("  to see ");
      color_putl ("Hi there.", yellow, blue,
		  attrs'(bold => True, ul => True,
			 rv => True, others => False));
   end print_usage;
begin
   ---------------------------------------------------------------------------
   std_out := Stream (Standard_Output);
   if Argument_Count = 0 then
      print_usage;
      return;
   end if;

   in_setting.fg_set := False;
   in_setting.bg_set := False;
   in_setting.attr := attrs'(others => False);
   -- process the arguments
   for i in 1 .. Argument_Count loop
      if Argument (i) = "-h" or else Argument (i) = "--help" then
	 print_usage;
	 return;
      end if;
      if Argument (i) = "off" then
	 put_reset;
	 return;
      end if;
      -- anyting shorting then 2 is wrong
      if Argument (i)'Length < 2 then
	 put_reset;
	 return;
      end if;
      if Argument (i) = "-l" or else Argument (i) = "--list" then
	 put_color_list;
	 return;
      end if;
      case Argument (i)(1) is
	 when 'b' =>
	    case Argument (i)'Length is
	       when 2 =>
		  in_setting.attr (attribute'Value (Argument (i))) := True;
	       when 4 | 5 =>
		  if Argument (i)(3) = 'a' or else -- black
		     Argument (i)(3) = 'u' then -- blue
		     if in_setting.fg_set then
			in_setting.bg_color := color'Value (Argument (i));
			in_setting.bg_set := True;
		     else
			in_setting.fg_color := color'Value (Argument (i));
			in_setting.fg_set := True;
		     end if;
		  else
		     in_setting.attr (attribute'Value (Argument (i))) := True;
		  end if;
	       when others =>
		  put_reset;
		  return;
	    end case;
	 when 'r' =>
	    case Argument (i)'Length is
	       when 2 | 8 =>
		  in_setting.attr (attribute'Value (Argument (i))) := True;
	       when 3 =>
		  if in_setting.fg_set then
		     in_setting.bg_color := color'Value (Argument (i));
		     in_setting.bg_set := True;
		  else
		     in_setting.fg_color := color'Value (Argument (i));
		     in_setting.fg_set := True;
		  end if;
	       when others =>
		  put_reset;
		  return;
	    end case;
	 when 'n' | 'f' | 'i' | 'u' =>
	    in_setting.attr (attribute'Value (Argument (i))) := True;
	 when 'l' =>
	    if Argument (i)(2) /= 't' then
	       put_reset;
	       return;
	    end if;
	    in_setting.attr (bold) := True;
	    in_setting.fg_color := color'Value
	       (Argument (i)(3 .. Argument (i)'Length));
	    in_setting.fg_set := True;
	 when 'g' | 'y' | 'm' | 'c' | 'w' =>
	    if in_setting.fg_set then
	       in_setting.bg_color := color'Value (Argument (i));
	       in_setting.bg_set := True;
	    else
	       in_setting.fg_color := color'Value (Argument (i));
	       in_setting.fg_set := True;
	    end if;
	 when others =>
	    put_reset;
	    return;
      end case;
   end loop;
   -- output attributes
   put_escape_sequence (in_setting);
-- when exception happened, just output a reset
exception
   when others =>
      put_reset;
end acolor;

-- vim: set sw=3 ts=8 sts=3:
