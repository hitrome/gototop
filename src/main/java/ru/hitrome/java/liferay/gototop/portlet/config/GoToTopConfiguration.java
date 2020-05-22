/*******************************************************************************
 * Copyright (C) 2020 Roman Novikov
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
package ru.hitrome.java.liferay.gototop.portlet.config;

import com.liferay.portal.configuration.metatype.annotations.ExtendedObjectClassDefinition;

import aQute.bnd.annotation.metatype.Meta;
import ru.hitrome.java.liferay.gototop.constants.GoToTopConstants;

/**
 * Configuration model interface.
 * 
 * @author Roman Novikov (rrl-software@mail.ru, http://hitrome.ru)
 *
 */
@ExtendedObjectClassDefinition(
	    category = "visual-tools",
	    scope = ExtendedObjectClassDefinition.Scope.PORTLET_INSTANCE
	)
@Meta.OCD(
		id = "ru.hitrome.java.liferay.gototop.portlet.config.GoToTopConfiguration",
		localization = "content/Language",
		name = "gototop-button-configuration"
		)
public interface GoToTopConfiguration {
	
	
	/**
	 * Button image path. If empty then an image will picked from the next properties
	 */
	@Meta.AD(
			required = false,
			name = GoToTopConstants.CONFIG_IMAGE_PATH_KEY,
			deflt = GoToTopConstants.DEFAULT_IMAGE_PATH
			)
	public String imagePath();
	
	/**
	 * Button image ID (from document library). The image will be shown if the "imagePath" is empty.
	 */
	@Meta.AD(
			required = false,
			name = GoToTopConstants.CONFIG_IMAGE_ID_KEY,
			deflt = GoToTopConstants.DEFAULT_IMAGE_ID
			)
	public long imageId();
	
	/**
	 * Color value of the button text.
	 */
	@Meta.AD(
			required = false,
			name = GoToTopConstants.CONFIG_COLOR_TEXT_KEY,
			deflt = GoToTopConstants.DEFAULT_CONFIG_COLOR_TEXT
			)
	public String colorText();
	
	/**
	 * Button text
	 */
	@Meta.AD(
			required = false,
			name = GoToTopConstants.CONFIG_BUTTON_TEXT_KEY,
			deflt = GoToTopConstants.DEFAULT_CONFIG_BUTTON_TEXT
			)
	public String buttonText();
	
	/**
	 * Button text font family
	 */
	@Meta.AD(
			required = false,
			name = GoToTopConstants.CONFIG_BUTTON_TEXT_FONT_FAMILY_KEY,
			deflt = GoToTopConstants.DEFAULT_CONFIG_BUTTON_TEXT_FONT_FAMILY
			)
	public String buttonTextFontFamily();
	
	/**
	 * Button text font size
	 */
	@Meta.AD(
			required = false,
			name = GoToTopConstants.CONFIG_BUTTON_TEXT_FONT_SIZE_KEY,
			deflt = GoToTopConstants.DEFAULT_CONFIG_BUTTON_TEXT_FONT_SIZE
			)
	public int buttonTextFontSize();
	
	/**
	 * Button text font style: bold
	 */
	@Meta.AD(
			required = false,
			name = GoToTopConstants.CONFIG_BUTTON_TEXT_FONT_STYLE_BOLD_KEY,
			deflt = GoToTopConstants.DEFAULT_CONFIG_BUTTON_TEXT_FONT_STYLE_BOLD
			)
	public boolean buttonTextFontStyleBold();
	
	/**
	 * Button text font style: italic
	 */
	@Meta.AD(
			required = false,
			name = GoToTopConstants.CONFIG_BUTTON_TEXT_FONT_STYLE_ITALIC_KEY,
			deflt = GoToTopConstants.DEFAULT_CONFIG_BUTTON_TEXT_FONT_STYLE_ITALIC
			)
	public boolean buttonTextFontStyleItalic();
	
	/**
	 * Button text font style: underline
	 */
	@Meta.AD(
			required = false,
			name = GoToTopConstants.CONFIG_BUTTON_TEXT_FONT_STYLE_UNDERLINE_KEY,
			deflt = GoToTopConstants.DEFAULT_CONFIG_BUTTON_TEXT_FONT_STYLE_UNDERLINE
			)
	public boolean buttonTextFontStyleUnderline();
	
	/**
	 * Button text decoration: capitalize
	 */
	@Meta.AD(
			required = false,
			name = GoToTopConstants.CONFIG_BUTTON_TEXT_DECORATION_CAPITALIZE_KEY,
			deflt = GoToTopConstants.DEFAULT_CONFIG_BUTTON_TEXT_DECORATION_CAPITALIZE
			)
	public boolean buttonTextDecorationCapitalize();
	
	/**
	 * Button width.
	 */
	@Meta.AD(
			required = false,
			name = GoToTopConstants.CONFIG_BUTTON_WIDTH_KEY,
			deflt = GoToTopConstants.DEFAULT_CONFIG_BUTTON_WIDTH
			)
	public int buttonWidth();
	
	/**
	 * Button height.
	 */
	@Meta.AD(
			required = false,
			name = GoToTopConstants.CONFIG_BUTTON_HEIGHT_KEY,
			deflt = GoToTopConstants.DEFAULT_CONFIG_BUTTON_HEIGHT
			)
	public int buttonHeight();
	
	/**
	 * Button horizontal alignment. True - left, false - right.
	 */
	@Meta.AD(
			required = false,
			name = GoToTopConstants.CONFIG_BUTTON_H_ALIGNMENT_KEY,
			deflt = GoToTopConstants.DEFAULT_CONFIG_BUTTON_H_ALIGNMENT
			)
	public boolean buttonHAlignment();
	
	/**
	 * Horizontal position of the button.
	 */
	@Meta.AD(
			required = false,
			name = GoToTopConstants.CONFIG_BUTTON_H_POSITION_KEY,
			deflt = GoToTopConstants.DEFAULT_CONFIG_BUTTON_H_POSITION
			)
	public int buttonHPosition();
	
	/**
	 * The button vertical alignment. If true then top-aligned.
	 */
	@Meta.AD(
			required = false,
			name = GoToTopConstants.CONFIG_BUTTON_V_ALIGNMENT_KEY,
			deflt = GoToTopConstants.DEFAULT_CONFIG_BUTTON_V_ALIGNMENT
			)
	public boolean buttonVAlignment();
	
	
	/**
	 * Vertical position of the button (top related).
	 */
	@Meta.AD(
			required = false,
			name = GoToTopConstants.CONFIG_BUTTON_V_POSITION_KEY,
			deflt = GoToTopConstants.DEFAULT_CONFIG_BUTTON_V_POSITION
			)
	public int buttonVPosition();
	
	/**
	 * Speed of the scrolling.
	 */
	@Meta.AD(
			required = false,
			name = GoToTopConstants.CONFIG_BUTTON_SCROLL_SPEED_KEY,
			deflt = GoToTopConstants.DEFAULT_CONFIG_BUTTON_SCROLL_SPEED
			)
	public int scrollSpeed();
	
	/**
	 * The button appearence pozition.
	 */
	@Meta.AD(
			required = false,
			name = GoToTopConstants.CONFIG_BUTTON_APPEARENCE_POSITION_KEY,
			deflt = GoToTopConstants.DEFAULT_CONFIG_BUTTON_APPEARENCE_POSITION
			)
	public int appearencePosition();
	
	/**
	 * Button & text transition time
	 */
	@Meta.AD(
			required = false,
			name = GoToTopConstants.CONFIG_BUTTON_TRANSITION_TIME_KEY,
			deflt = GoToTopConstants.DEFAULT_CONFIG_BUTTON_TRANSITION_TIME
			)
	public int transitionTime();
	
	/**
	 * Space between button and text
	 */
	@Meta.AD(
			required = false,
			name = GoToTopConstants.CONFIG_BUTTON_TEXTBUTTON_SPACE_KEY,
			deflt = GoToTopConstants.DEFAULT_CONFIG_BUTTON_TEXTBUTTON_SPACE
			)
	public int textButtonSpace();

}
