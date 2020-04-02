from ranger.gui.colorscheme import ColorScheme
from ranger.gui.color import default_colors, reverse, normal 

class Sherlock(ColorScheme):
    white = 7
    beige = 223
    yellow = 179
    red = 167
    blue = 109
    gray = 243
    gray_bg = 234

    def use(self, context):
        fg, bg, attr = default_colors

        if context.reset:
            return default_colors

        elif context.in_browser:
            if context.selected:
                attr = reverse
            else:
                attr = normal

            if context.empty or context.error:
                fg = self.white
                bg = self.red

            if context.directory:
                fg = self.yellow

            if context.link:
                fg = self.gray if context.good else self.red

            if context.image:
                fg = self.blue
            if context.video:
                fg = self.blue
            if context.audio:
                fg = self.blue

            if context.main_column:
                if context.marked:
                    bg = self.gray_bg

            if context.tag_marker and not context.selected:
                fg = self.red

            if not context.selected and (context.cut or context.copied):
                fg = self.gray
                bg = self.gray_bg

            if context.border:
                fg = 236

        elif context.in_titlebar:
            if context.hostname:
                fg = self.beige if context.good else self.red
            elif context.file:
                fg = self.white
            elif context.directory:
                fg = self.gray
            elif context.tab and context.good:
                fg = self.blue

        elif context.in_statusbar:
            if context.permissions:
                if context.good:
                    fg = self.gray
                elif context.bad:
                    fg = self.red

            if context.mtime:
                fg = self.blue
            if context.owner or context.group:
                fg = self.gray
            if context.nlink:
                fg = self.gray
            if context.link:
                fg = self.yellow
            if context.all:
                fg = self.red
            if context.message and context.bad:
                fg = self.red

        return fg, bg, attr
