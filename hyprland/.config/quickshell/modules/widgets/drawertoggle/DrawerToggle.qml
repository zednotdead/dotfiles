import QtQuick
import qs.config
import qs.services.drawer

MouseArea {
    hoverEnabled: true
    onClicked: function () {
        DrawerState.toggle();
    }

    Rectangle {
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.verticalCenter: parent.verticalCenter

        implicitHeight: parent.height / 3
        implicitWidth: parent.width
        radius: this.height

        color: {
            if (parent.pressed) {
                return Theme.buttonActive;
            } else if (parent.containsMouse) {
                return Theme.buttonHover;
            }
            return Theme.buttonInactive;
        }
    }
}
