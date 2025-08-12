pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io

Singleton {
    id: root

    property alias barBackground: adapter.background

    property color workspaceFocused: this.color05
    property color workspaceFocusedHover: Qt.lighter(this.workspaceFocused, 1.2)
    property color workspaceUnfocused: Qt.darker(this.workspaceFocused, 1.5)
    property color workspaceUnfocusedHover: Qt.lighter(this.workspaceUnfocused, 1.2)

    property alias background: adapter.background
    property alias foreground: adapter.foreground
    property alias color00: adapter.color0
    property alias color01: adapter.color1
    property alias color02: adapter.color2
    property alias color03: adapter.color3
    property alias color04: adapter.color4
    property alias color05: adapter.color5
    property alias color06: adapter.color6
    property alias color07: adapter.color7
    property alias color08: adapter.color8
    property alias color09: adapter.color9
    property alias color10: adapter.color10
    property alias color11: adapter.color11
    property alias color12: adapter.color12
    property alias color13: adapter.color13
    property alias color14: adapter.color14
    property alias color15: adapter.color15

    FileView {
        path: `/home/zed/.config/quickshell/config/theme.json`
        watchChanges: true
        onFileChanged: reload()
        onAdapterUpdated: writeAdapter()

        onLoaded: root.ready = true
        onLoadFailed: error => {
            if (error == FileViewError.FileNotFound) {
                writeAdapter();
            }
        }

        JsonAdapter {
            id: adapter
            property string background: "#000"
            property string foreground: "#000"
            property string color0: "#000"
            property string color1: "#000"
            property string color2: "#000"
            property string color3: "#000"
            property string color4: "#000"
            property string color5: "#000"
            property string color6: "#000"
            property string color7: "#000"
            property string color8: "#000"
            property string color9: "#000"
            property string color10: "#000"
            property string color11: "#000"
            property string color12: "#000"
            property string color13: "#000"
            property string color14: "#000"
            property string color15: "#000"
        }
    }

    property bool ready: false
}
