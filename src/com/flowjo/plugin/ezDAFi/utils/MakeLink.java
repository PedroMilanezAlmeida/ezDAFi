package com.flowjo.plugin.ezDAFi.utils;

import com.treestar.lib.gui.panels.FJLabel;

import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

public class MakeLink {

    public FJLabel makeCLink(String html) {

        String htmlShort = (html.replace("https://", "").replace("http://", "")).replace("mailto:", "");
        FJLabel CLink = new FJLabel(htmlShort);
        CLink.setForeground(Color.BLUE.darker());
        CLink.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        CLink.addMouseListener(new MouseAdapter() {
            public void mouseClicked(MouseEvent e) {
                // the user clicks on the label
                try {
                    Desktop.getDesktop().browse(new URI(html));
                } catch (IOException | URISyntaxException e1) {
                    e1.printStackTrace();
                }
            }

            public void mouseEntered(MouseEvent e) {
                // the mouse has entered the label
                CLink.setText("<html><a href='" + html + "'>" + htmlShort + "</a></html>");
            }

            public void mouseExited(MouseEvent e) {
                // the mouse has exited the label
                CLink.setText(htmlShort);
            }
        });
        return CLink;
    }

}
