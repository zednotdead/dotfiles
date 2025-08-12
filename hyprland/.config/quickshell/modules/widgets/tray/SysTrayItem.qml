import Quickshell
import Quickshell.Widgets
import QtQuick.Layouts
import Quickshell.Services.SystemTray

WrapperMouseArea {
    id: root
    required property SystemTrayItem item
    property var bar: QsWindow.window
    acceptedButtons: Qt.LeftButton | Qt.RightButton
    Layout.fillHeight: true
    implicitWidth: 24

    onClicked: function (ev) {
        switch (ev.button) {
        case Qt.LeftButton:
            item.activate();
            break;
        case Qt.RightButton:
            if (item.hasMenu) menu.open()
            break;
        }
    }

    QsMenuAnchor {
        id: menu

        menu: root.item.menu
        anchor.window: root.QsWindow.window
        anchor.rect.x: root.x + root.bar.width
        anchor.rect.y: root.y
        anchor.rect.height: root.height
        anchor.edges: Edges.Bottom
    }

    IconImage {
        id: trayIcon
        source: root.item.icon
        height: parent.height
    }
}
