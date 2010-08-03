"""
Handling of custom tags. This script can be used as is or as a basis for
 your own documentation tags.

The list of custom tags is:
- description
- summary
- parameter (attribute "name" is expected)
- exception
- seealso
- c_version
- group (this builds a custom index from all <group>A Group</group> tags)
- screenshot (this builds a custom index with all screenshots, and also
   display the actual image instead of the tag).
"""

import GPS, re, os
import shutil

class ScreenshotTagHandler (GPS.DocgenTagHandler):
   """Handling for screenshots"""
   def __init__ (self):
      GPS.DocgenTagHandler.__init__ (
        self, "screenshot",
        on_match = self.on_match,
        on_start=self.on_start,
        on_exit=self.on_exit)

   def on_start (self, docgen):
      self.pictureslist = {}

   def on_match (self, docgen, attrs, value, entity_name, entity_href):
      GPS.Console ("Messages").write ("Screenshot: %s\n" % (value))
      file = docgen.get_current_file()
      dir = os.path.normpath (
         os.path.join (GPS.Project.root().file().directory(),
                       "..", "docs", "gtkada_rm"))
      fullfile = os.path.join (dir, value)

      try:
         os.stat(fullfile)
         pict = value
      except:
         try:
            os.stat(fullfile+".png")
            pict = value + ".png"
         except:
            try:
               os.stat(fullfile+".jpg")
               pict = value + ".jpg"
            except:
               GPS.Console ("Messages").write ("could not find screenshot %s\n" % (fullfile))
               return ""

      if not os.path.exists ("pics"):
           os.mkdir ("pics")

      shutil.copy (os.path.join (dir, pict), "pics")

      img = """<img src="%s" alt="%s" style="border: 0px;"/>""" % (("pics" + "/" + pict), pict)

      self.pictureslist[entity_name] = [entity_href, img]
      return """</div>
        <div class='profile'>
          <h3>Screenshot</h3>
          %s
        </div>
        <div class='comment'>""" % (img)

   def on_exit (self, docgen):
      if len (self.pictureslist) == 0:
         return

      # first print the right-side box containing the group index
      content=""
      content += """<div class='default' id='rightSide'>"""
      content += """<div id='rightSideInside'>"""
      content += """<div id='Index'>"""
      content += """<h2>Index</h2>"""
      content += """<ul>"""
      n = 0
      for pict in sorted(self.pictureslist.keys()):
         content += """<li><a href="#%d">%s</a></li>""" % (n, pict)
         n += 1
      content += """</ul></div></div></div>"""

      content += """<div class='default' id='documentation'>"""
      content += """<div class="title">Widget Screenshots</div>"""
      n = 0
      for pict in sorted(self.pictureslist.keys()):
         content += """
            <div class='subprograms'>
              <div class='class'>
                <a name="%d"></a>
                <h3>%s</h3>
                <div class='comment'>
                  <a href="%s">%s</a>
                </div>
              </div>
            </div>""" % (n, pict, self.pictureslist[pict][0], self.pictureslist[pict][1])
         n += 1
      content += """</div>"""

      docgen.generate_index_file ("Widget Screenshots", "screenshots.html", content);

def on_gps_start (hook):
   GPS.Docgen.register_tag_handler (ScreenshotTagHandler ())

GPS.Hook ("gps_started").add (on_gps_start)
