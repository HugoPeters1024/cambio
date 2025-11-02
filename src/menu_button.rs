use bevy::{color::palettes::css::*, prelude::*};

const NORMAL_BUTTON: Color = Color::srgb(0.15, 0.15, 0.15);
const HOVERED_BUTTON: Color = Color::srgb(0.25, 0.25, 0.25);
const PRESSED_BUTTON: Color = Color::srgb(0.35, 0.75, 0.35);

#[derive(EntityEvent)]
pub struct MenuButtonClicked(Entity);

#[derive(Component)]
#[require(Node, Button)]
pub struct MenuButton(pub String);

pub struct MenuButtonPlugin;

impl Plugin for MenuButtonPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, button_system);

        app.add_observer(on_button_spawn);
    }
}

fn on_button_spawn(
    trigger: On<Add, MenuButton>,
    mut commands: Commands,
    menu_buttons: Query<&MenuButton>,
) {
    let Ok(menu_button) = menu_buttons.get(trigger.event_target()) else {
        return;
    };

    commands.entity(trigger.event_target()).insert((
        Button,
        Node {
            width: Val::Px(180.0),
            height: Val::Px(65.0),
            border: UiRect::all(Val::Px(5.0)),
            // horizontally center child text
            justify_content: JustifyContent::Center,
            // vertically center child text
            align_items: AlignItems::Center,
            ..default()
        },
        BorderColor::all(Color::BLACK),
        BorderRadius::MAX,
        BackgroundColor(NORMAL_BUTTON),
        children![(
            Text::new(menu_button.0.as_str()),
            TextFont {
                font_size: 21.0,
                ..default()
            },
            TextColor(Color::srgb(0.9, 0.9, 0.9)),
            TextShadow::default(),
        )],
    ));
}

fn button_system(
    mut commands: Commands,
    mut interaction_query: Query<
        (Entity, &Interaction, &mut BackgroundColor, &mut BorderColor),
        Changed<Interaction>,
    >,
) {
    for (entity, interaction, mut color, mut border_color) in &mut interaction_query {
        match *interaction {
            Interaction::Pressed => {
                *color = PRESSED_BUTTON.into();
                border_color.set_all(RED);
                commands.trigger(MenuButtonClicked(entity));
            }
            Interaction::Hovered => {
                *color = HOVERED_BUTTON.into();
                border_color.set_all(Color::WHITE);
            }
            Interaction::None => {
                *color = NORMAL_BUTTON.into();
                border_color.set_all(Color::BLACK);
            }
        }
    }
}
