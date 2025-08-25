import QtQuick
import Quickshell.Widgets
import Quickshell.Services.Notifications

Item {
    id: root
    required property string app
    required property string title

    Text {
        text: root.title + root.app
    }
}
