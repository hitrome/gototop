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

import com.liferay.document.library.kernel.service.DLAppLocalService;
import com.liferay.portal.kernel.configuration.Configuration;
import com.liferay.portal.kernel.exception.PortalException;
import com.liferay.portal.kernel.util.Validator;

import java.util.Locale;
import java.util.Map;

import ru.hitrome.java.liferay.gototop.constants.GoToTopConstants;
import ru.hitrome.java.liferay.gototop.portlet.config.exceptions.AppearencePositionException;
import ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonHAlignmentException;
import ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonHPositionException;
import ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonHeightException;
import ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonTextDecorationCapitalizeException;
import ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonTextException;
import ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonTextFontFamilyException;
import ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonTextFontSizeException;
import ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonTextFontStyleBoldException;
import ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonTextFontStyleItalicException;
import ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonTextFontStyleUnderlineException;
import ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonVAlignmentException;
import ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonVPositionException;
import ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonWidthException;
import ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ColorTextException;
import ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ImageIdException;
import ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ImagePathException;
import ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ScrollSpeedException;
import ru.hitrome.java.liferay.gototop.portlet.config.exceptions.TextButtonSpaceException;
import ru.hitrome.java.liferay.gototop.portlet.config.exceptions.TransitionTimeException;

/**
 * Validators class incapsulates methods for validation configuration values before they are persisted in database.
 * 
 * @author Roman Novikov (rrl-software@mail.ru, http://hitrome.ru)
 *
 */
public class Validators {
	
	Validators(Configuration configuration, DLAppLocalService dlAppLocalService) {
		_configuration = configuration;
		_dlAppLocalService = dlAppLocalService;
	}
	
	public void validateImagePath(String imagePath) throws PortalException {
		if (!imagePath.isEmpty() && (imagePath.length() > Integer.parseInt(_configuration.get(GoToTopConstants.MAX_CONFIG_IMAGE_PATH))
				|| !Validator.isUrl(imagePath, true))) {
			throw new ImagePathException();
		}
	}
	
	public void validateImageId(String imageId) throws PortalException {
		long imgId = 0;
		try {
			imgId = Long.parseLong(imageId);
		} catch (NumberFormatException e) {
			throw new ImageIdException();
		}
		if (imgId < 0) {
			throw new ImageIdException();
		}
		if (imgId > 0) {
			try {
				_dlAppLocalService.getFileEntry(imgId);
			} catch (PortalException e) {
				throw new ImageIdException();
			}
		}
	}
	
	public void validateColorText (String colorText) throws PortalException {
		if (!colorText.isEmpty() && (colorText.length() > Integer.parseInt(_configuration.get(GoToTopConstants.MAX_CONFIG_COLOR_TEXT_LENGTH))
				|| !colorText.matches("^#[0-9a-fA-F]*$"))) {
			throw new ColorTextException();
		}
	}
	
	public void validateButtonText (Map<Locale, String> buttonText) throws PortalException {
		int constraint = Integer.parseInt(_configuration.get(GoToTopConstants.MAX_CONFIG_BUTTON_TEXT_LENGTH));
		try {
			buttonText.forEach((a, b) -> {
				if (Validator.isNotNull(b) && b.length() > constraint) {
					throw new RuntimeException();
				}
			});
		} catch (RuntimeException re) {
			throw new ButtonTextException();
		}
	}
	
	public void validateButtonTextFontFamily (int buttonTextFontFamily) throws PortalException {
		if (buttonTextFontFamily < 0 || buttonTextFontFamily > GoToTopConstants.FONT_FAMILIES.length - 1 ) {
			throw new ButtonTextFontFamilyException();
		}
	}
	
	public void validateButtonTextFontSize (String buttonTextFontSize) throws PortalException {
		int value = strToInt(buttonTextFontSize, new ButtonTextFontSizeException());
		if (value < 0 || value > Integer.parseInt(_configuration.get(GoToTopConstants.MAX_CONFIG_BUTTON_TEXT_FONT_SIZE))) {
			throw new ButtonTextFontSizeException();
		}
	}
	
	public void validateButtonTextFontStyleBold (String buttonTextFontStyleBold) throws PortalException {
		checkBoolean(buttonTextFontStyleBold, new ButtonTextFontStyleBoldException());
	}
	
	public void validateButtonTextFontStyleItalic (String buttonTextFontStyleItalic) throws PortalException {
		checkBoolean(buttonTextFontStyleItalic, new ButtonTextFontStyleItalicException());
	}
	
	public void validateButtonTextFontStyleUnderline (String buttonTextFontStyleUnderline) throws PortalException {
		checkBoolean(buttonTextFontStyleUnderline, new ButtonTextFontStyleUnderlineException());
	}
	
	public void validateButtonTextDecorationCapitalize (String buttonTextDecorationCapitalize) throws PortalException {
		checkBoolean(buttonTextDecorationCapitalize, new ButtonTextDecorationCapitalizeException());
	}
	
	public void validateButtonWidth (String buttonWidth) throws PortalException {
		int value = strToInt(buttonWidth, new ButtonWidthException());
		if (value < 0 || value > Integer.parseInt(_configuration.get(GoToTopConstants.MAX_CONFIG_BUTTON_WIDTH))) {
			throw new ButtonWidthException();
		}
	}
	
	public void validateButtonHeight (String buttonHeight) throws PortalException {
		int value = strToInt(buttonHeight, new ButtonHeightException());
		if (value < 0 || value > Integer.parseInt(_configuration.get(GoToTopConstants.MAX_CONFIG_BUTTON_HEIGHT))) {
			throw new ButtonHeightException();
		}
	}
	
	public void validateButtonHAlignment (String buttonHAlignment) throws PortalException {
		checkBoolean(buttonHAlignment, new ButtonHAlignmentException());
	}
	
	public void validateButtonHPosition (String buttonHPosition) throws PortalException {
		int value = strToInt(buttonHPosition, new ButtonHPositionException());
		if (value < 0 || value > Integer.parseInt(_configuration.get(GoToTopConstants.MAX_CONFIG_BUTTON_H_POSITION))) {
			throw new ButtonHPositionException();
		}
	}
	
	public void validateButtonVAlignment (String buttonVAlignment) throws PortalException {
		checkBoolean(buttonVAlignment, new ButtonVAlignmentException());
	}
	
	public void validateButtonVPosition (String buttonVPosition) throws PortalException {
		int value = strToInt(buttonVPosition, new ButtonVPositionException());
		if (value < 0 || value > Integer.parseInt(_configuration.get(GoToTopConstants.MAX_CONFIG_BUTTON_V_POSITION))) {
			throw new ButtonVPositionException();
		}
	}
	
	public void validateScrollSpeed (String scrollSpeed) throws PortalException {
		int value = strToInt(scrollSpeed, new ScrollSpeedException());
		if (value < 0 || value > Integer.parseInt(_configuration.get(GoToTopConstants.MAX_CONFIG_SCROLL_SPEED))) {
			throw new ScrollSpeedException();
		}
	}
	
	public void validateAppearencePosition (String appearencePosition) throws PortalException {
		int value = strToInt(appearencePosition, new AppearencePositionException());
		if (value < 0 || value > Integer.parseInt(_configuration.get(GoToTopConstants.MAX_CONFIG_APPEARENCE_POSITION))) {
			throw new AppearencePositionException();
		}
	}
	
	public void validateTransitionTime (String transitionTime) throws PortalException {
		int value = strToInt(transitionTime, new TransitionTimeException());
		if (value < 0 || value > Integer.parseInt(_configuration.get(GoToTopConstants.MAX_CONFIG_TRANSITION_TIME))) {
			throw new TransitionTimeException();
		}
	}
	
	public void validateTextButtonSpace (String textButtonSpace) throws PortalException {
		if (Math.abs(strToInt(textButtonSpace, new TextButtonSpaceException()))
				> Integer.parseInt(_configuration.get(GoToTopConstants.MAX_CONFIG_TEXT_BUTTON_SPACE))) {
			throw new TextButtonSpaceException();
		}
	}
	
	public int strToInt(String value, PortalException e) throws PortalException {
		try {
			return Integer.parseInt(value);
		} catch (NumberFormatException nfe) {
			throw e;
		}
	}
	
	public void checkBoolean(String value, PortalException e) throws PortalException {
		if (value.length() > Boolean.FALSE.toString().length() || !(value.equals(Boolean.TRUE.toString())
				|| value.equals(Boolean.FALSE.toString()))) {
			throw e;
		}
	}
	
	
	
	private Configuration _configuration;
	private DLAppLocalService _dlAppLocalService;

}
