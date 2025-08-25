pragma ComponentBehavior: Bound
import QtQuick
import Quickshell
import Quickshell.Services.Notifications

PanelWindow {
    id: root
    exclusionMode: ExclusionMode.Ignore

    implicitWidth: 600
    implicitHeight: 200

    visible: true

    anchors {
        bottom: true
        right: true
    }

    margins {
        bottom: 40
    }

    ListModel {
        id: notifs
    }
}
