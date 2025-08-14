pragma ComponentBehavior: Bound
import Quickshell
import Quickshell.Widgets
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import qs.services.drawer
import qs.config
import qs.modules.decorations.corner
import qs.modules.widgets.tab
import qs.modules.drawer.pages

PopupWindow {
    id: root

    required property int drawerWidth
    required property int drawerHeight

    property int cornerSize: 20
    property int cornerWidthAdjustment: cornerSize * 2
    property int drawerAnimationSpeed: 300

    implicitWidth: drawerWidth + cornerWidthAdjustment
    implicitHeight: 500
    visible: DrawerState.visible

    color: "transparent"

    Item {
        id: window
        implicitWidth: root.drawerWidth
        implicitHeight: root.height

        Rectangle {
            id: windowbg
            anchors.centerIn: parent
            implicitWidth: parent.width - root.cornerWidthAdjustment
            implicitHeight: parent.height
            bottomLeftRadius: 20
            bottomRightRadius: 20

            color: Theme.barBackground

            WrapperItem {
                margin: 10
                anchors.fill: parent

                ClippingRectangle {
                    radius: 10
                    color: Theme.barBackground

                    ColumnLayout {
                        anchors.fill: parent

                        StackLayout {
                            Layout.fillWidth: true
                            Layout.fillHeight: true
                            currentIndex: bar.currentIndex

                            BasePage {
                                id: discoverTab
                                title: "Discover"
                            }

                            Notifications {
                                id: notificationTab
                            }

                            Settings {
                                id: settingsTab
                            }
                        }

                        TabBar {
                            id: bar

                            Layout.fillWidth: true

                            background: Theme.background.toString()

                            StyledTabButton {
                                text: discoverTab.title
                            }
                            StyledTabButton {
                                text: notificationTab.title
                            }

                            StyledTabButton {
                                text: settingsTab.title
                            }
                        }
                    }
                }
            }
        }

        Corner {
            size: root.cornerSize
            positionX: "right"

            anchors.top: window.top
            anchors.left: window.left
        }

        Corner {
            size: root.cornerSize
            positionX: "left"

            anchors.top: window.top
            anchors.right: window.right
        }
    }

    ParallelAnimation {
        YAnimator {
            target: window
            from: -root.drawerHeight
            to: 0
            duration: root.drawerAnimationSpeed
            easing.type: Easing.InOutQuad
        }
        running: DrawerState.shouldOpen
        onFinished: function () {
            DrawerState.reset(true);
        }
    }

    ParallelAnimation {
        YAnimator {
            target: window
            from: 0
            to: -root.drawerHeight
            duration: root.drawerAnimationSpeed
            easing.type: Easing.InOutQuad
        }
        running: DrawerState.shouldClose
        onFinished: function () {
            DrawerState.reset(false);
        }
    }
}
