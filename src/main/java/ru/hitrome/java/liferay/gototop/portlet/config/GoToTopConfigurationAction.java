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
import com.liferay.document.library.util.DLURLHelper;
import com.liferay.item.selector.ItemSelector;
import com.liferay.portal.configuration.metatype.bnd.util.ConfigurableUtil;
import com.liferay.portal.kernel.configuration.Configuration;
import com.liferay.portal.kernel.configuration.ConfigurationFactoryUtil;
import com.liferay.portal.kernel.log.Log;
import com.liferay.portal.kernel.log.LogFactoryUtil;
import com.liferay.portal.kernel.module.configuration.ConfigurationProvider;
import com.liferay.portal.kernel.portlet.ConfigurationAction;
import com.liferay.portal.kernel.portlet.DefaultConfigurationAction;
import com.liferay.portal.kernel.portlet.PortletPreferencesFactoryUtil;
import com.liferay.portal.kernel.service.PortletLocalService;
import com.liferay.portal.kernel.theme.ThemeDisplay;
import com.liferay.portal.kernel.util.LocaleUtil;
import com.liferay.portal.kernel.util.LocalizationUtil;
import com.liferay.portal.kernel.util.ParamUtil;
import com.liferay.portal.kernel.util.PortalClassLoaderUtil;
import com.liferay.portal.kernel.util.PortalUtil;
import com.liferay.portal.kernel.util.WebKeys;

import java.util.Arrays;
import java.util.Locale;
import java.util.Map;
import javax.portlet.ActionRequest;
import javax.portlet.ActionResponse;
import javax.portlet.PortletConfig;
import javax.portlet.PortletPreferences;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Modified;
import org.osgi.service.component.annotations.Reference;

import ru.hitrome.java.liferay.gototop.constants.GoToTopConstants;
import ru.hitrome.java.liferay.gototop.constants.GototopPortletKeys;

/**
 * Configuration MVC controller class.
 * 
 * @author Roman Novikov (rrl-software@mail.ru, http://hitrome.ru)
 *
 */
@Component(
		immediate = true,
		property = {
				"javax.portlet.name=" + GototopPortletKeys.GOTOTOP
		},
		service = ConfigurationAction.class
	)
public class GoToTopConfigurationAction extends DefaultConfigurationAction {
	
	@Override
	public String getJspPath(HttpServletRequest request) {
		return "/configuration.jsp";
	}
	
	@Override
	public void processAction(
		PortletConfig portletConfig, ActionRequest actionRequest,
		ActionResponse actionResponse)
		throws Exception {
		
		// Get values
		String imagePath = ParamUtil.getString(actionRequest, GoToTopConstants.CONFIG_IMAGE_PATH_KEY,
				GoToTopConstants.DEFAULT_IMAGE_PATH);
		String imageId = ParamUtil.getString(actionRequest, GoToTopConstants.CONFIG_IMAGE_ID_KEY,
				GoToTopConstants.DEFAULT_IMAGE_ID);
		String colorText= ParamUtil.getString(actionRequest, GoToTopConstants.CONFIG_COLOR_TEXT_KEY,
				GoToTopConstants.DEFAULT_CONFIG_BUTTON_TEXT);
		Map<Locale, String> buttonText = LocalizationUtil.getLocalizationMap(actionRequest,
				GoToTopConstants.CONFIG_BUTTON_TEXT_KEY);
		int buttonTextFontFamily = ParamUtil.getInteger(actionRequest, 
				GoToTopConstants.CONFIG_BUTTON_TEXT_FONT_FAMILY_KEY,
				Arrays.asList(GoToTopConstants.FONT_FAMILIES).indexOf(GoToTopConstants.DEFAULT_CONFIG_BUTTON_TEXT_FONT_FAMILY));
		String buttonTextFontSize = ParamUtil.getString(actionRequest, 
				GoToTopConstants.CONFIG_BUTTON_TEXT_FONT_SIZE_KEY,
				GoToTopConstants.DEFAULT_CONFIG_BUTTON_TEXT_FONT_SIZE);
		String buttonTextFontStyleBold = ParamUtil.getString(actionRequest, 
				GoToTopConstants.CONFIG_BUTTON_TEXT_FONT_STYLE_BOLD_KEY,
				GoToTopConstants.DEFAULT_CONFIG_BUTTON_TEXT_FONT_STYLE_BOLD);
		String buttonTextFontStyleItalic = ParamUtil.getString(actionRequest,
				GoToTopConstants.CONFIG_BUTTON_TEXT_FONT_STYLE_ITALIC_KEY,
				GoToTopConstants.DEFAULT_CONFIG_BUTTON_TEXT_FONT_STYLE_ITALIC);
		String buttonTextFontStyleUnderline = ParamUtil.getString(actionRequest,
				GoToTopConstants.CONFIG_BUTTON_TEXT_FONT_STYLE_UNDERLINE_KEY,
				GoToTopConstants.DEFAULT_CONFIG_BUTTON_TEXT_DECORATION_CAPITALIZE);
		String buttonTextDecorationCapitalize = ParamUtil.getString(actionRequest,
				GoToTopConstants.CONFIG_BUTTON_TEXT_DECORATION_CAPITALIZE_KEY,
				GoToTopConstants.DEFAULT_CONFIG_BUTTON_TEXT_DECORATION_CAPITALIZE);
		String buttonWidth = ParamUtil.getString(actionRequest, GoToTopConstants.CONFIG_BUTTON_WIDTH_KEY,
				GoToTopConstants.DEFAULT_CONFIG_BUTTON_WIDTH);
		String buttonHeight = ParamUtil.getString(actionRequest, GoToTopConstants.CONFIG_BUTTON_HEIGHT_KEY,
				GoToTopConstants.DEFAULT_CONFIG_BUTTON_HEIGHT);
		String buttonHAlignment = ParamUtil.getString(actionRequest, GoToTopConstants.CONFIG_BUTTON_H_ALIGNMENT_KEY,
				GoToTopConstants.DEFAULT_CONFIG_BUTTON_H_ALIGNMENT);
		String buttonHPosition = ParamUtil.getString(actionRequest, GoToTopConstants.CONFIG_BUTTON_H_POSITION_KEY,
				GoToTopConstants.DEFAULT_CONFIG_BUTTON_H_POSITION);
		String buttonVAlignment = ParamUtil.getString(actionRequest, GoToTopConstants.CONFIG_BUTTON_V_ALIGNMENT_KEY,
				GoToTopConstants.DEFAULT_CONFIG_BUTTON_V_ALIGNMENT);
		String buttonVPosition = ParamUtil.getString(actionRequest, GoToTopConstants.CONFIG_BUTTON_V_POSITION_KEY,
				GoToTopConstants.DEFAULT_CONFIG_BUTTON_V_POSITION);
		String scrollSpeed = ParamUtil.getString(actionRequest, GoToTopConstants.CONFIG_BUTTON_SCROLL_SPEED_KEY,
				GoToTopConstants.DEFAULT_CONFIG_BUTTON_SCROLL_SPEED);
		String appearencePosition = ParamUtil.getString(actionRequest, GoToTopConstants.CONFIG_BUTTON_APPEARENCE_POSITION_KEY,
				GoToTopConstants.DEFAULT_CONFIG_BUTTON_APPEARENCE_POSITION);
		String transitionTime = ParamUtil.getString(actionRequest, GoToTopConstants.CONFIG_BUTTON_TRANSITION_TIME_KEY,
				GoToTopConstants.DEFAULT_CONFIG_BUTTON_TRANSITION_TIME);
		String textButtonSpace = ParamUtil.getString(actionRequest, GoToTopConstants.CONFIG_BUTTON_TEXTBUTTON_SPACE_KEY,
				GoToTopConstants.DEFAULT_CONFIG_BUTTON_TEXTBUTTON_SPACE);
		
		
		
		// Validate values
		_validators.validateImagePath(imagePath);
		_validators.validateImageId(imageId);
		_validators.validateColorText(colorText);
		_validators.validateButtonText(buttonText);
		_validators.validateButtonTextFontFamily(buttonTextFontFamily);
		_validators.validateButtonTextFontSize(buttonTextFontSize);
		_validators.validateButtonTextFontStyleBold(buttonTextFontStyleBold);
		_validators.validateButtonTextFontStyleItalic(buttonTextFontStyleItalic);
		_validators.validateButtonTextFontStyleUnderline(buttonTextFontStyleUnderline);
		_validators.validateButtonTextDecorationCapitalize(buttonTextDecorationCapitalize);
		_validators.validateButtonWidth(buttonWidth);
		_validators.validateButtonHeight(buttonHeight);
		_validators.validateButtonHAlignment(buttonHAlignment);
		_validators.validateButtonHPosition(buttonHPosition);
		_validators.validateButtonVAlignment(buttonVAlignment);
		_validators.validateButtonVPosition(buttonVPosition);
		_validators.validateScrollSpeed(scrollSpeed);
		_validators.validateAppearencePosition(appearencePosition);
		_validators.validateTransitionTime(transitionTime);
		_validators.validateTextButtonSpace(textButtonSpace);

		
		// Persist values
		ThemeDisplay themeDisplay = (ThemeDisplay) actionRequest.getAttribute(WebKeys.THEME_DISPLAY);
		Locale locale = PortalUtil.getSiteDefaultLocale(themeDisplay.getScopeGroupId());
		setPreference(actionRequest, "imagePath", imagePath);
		setPreference(actionRequest, "imageId", imageId);
		setPreference(actionRequest, "colorText", colorText);
		setPreference(actionRequest, "buttonText", LocalizationUtil.updateLocalization(buttonText, "", "buttonText",
				LocaleUtil.toLanguageId(locale)));
		setPreference(actionRequest, "buttonTextFontFamily", GoToTopConstants.FONT_FAMILIES[buttonTextFontFamily]);
		setPreference(actionRequest, "buttonTextFontSize", buttonTextFontSize);
		setPreference(actionRequest, "buttonTextFontStyleBold", buttonTextFontStyleBold);
		setPreference(actionRequest, "buttonTextFontStyleItalic", buttonTextFontStyleItalic);
		setPreference(actionRequest, "buttonTextFontStyleUnderline", buttonTextFontStyleUnderline);
		setPreference(actionRequest, "buttonTextDecorationCapitalize", buttonTextDecorationCapitalize);
		setPreference(actionRequest, "buttonWidth", buttonWidth);
		setPreference(actionRequest, "buttonHeight", buttonHeight);
		setPreference(actionRequest, "buttonHAlignment", buttonHAlignment);
		setPreference(actionRequest, "buttonHPosition", buttonHPosition);
		setPreference(actionRequest, "buttonVAlignment", buttonVAlignment);
		setPreference(actionRequest, "buttonVPosition", buttonVPosition);
		setPreference(actionRequest, "scrollSpeed", scrollSpeed);
		setPreference(actionRequest, "appearencePosition", appearencePosition);
		setPreference(actionRequest, "transitionTime", transitionTime);
		setPreference(actionRequest, "textButtonSpace", textButtonSpace);
		
		
		super.processAction(portletConfig, actionRequest, actionResponse);
		
	}
	
	@Override
    public void include(
        PortletConfig portletConfig, HttpServletRequest httpServletRequest,
        HttpServletResponse httpServletResponse) throws Exception {
		
		ThemeDisplay themeDisplay = (ThemeDisplay) httpServletRequest.getAttribute(WebKeys.THEME_DISPLAY);
		String portletId = ParamUtil.getString(httpServletRequest, "portletResource");

		// Workaround of missing defaults problem
		PortletPreferences preferences = PortletPreferencesFactoryUtil.getLayoutPortletSetup(
				themeDisplay.getLayout(), portletId);
		if (preferences.getMap().isEmpty()) {
			preferences.setValue("imagePath", _goToTopConfiguration.imagePath());
			preferences.setValue("imageId", String.valueOf(_goToTopConfiguration.imageId()));
			preferences.setValue("colorText", _goToTopConfiguration.colorText());
			preferences.setValue("buttonText", _goToTopConfiguration.buttonText());
			preferences.setValue("buttonTextFontFamily", _goToTopConfiguration.buttonTextFontFamily());
			preferences.setValue("buttonTextFontSize", String.valueOf(_goToTopConfiguration.buttonTextFontSize()));
			preferences.setValue("buttonTextFontStyleBold", String.valueOf(_goToTopConfiguration.buttonTextFontStyleBold()));
			preferences.setValue("buttonTextFontStyleItalic", String.valueOf(_goToTopConfiguration.buttonTextFontStyleItalic()));
			preferences.setValue("buttonTextFontStyleUnderline", String.valueOf(_goToTopConfiguration.buttonTextFontStyleUnderline()));
			preferences.setValue("buttonTextDecorationCapitalize", String.valueOf(_goToTopConfiguration.buttonTextDecorationCapitalize()));
			preferences.setValue("buttonWidth", String.valueOf(_goToTopConfiguration.buttonWidth()));
			preferences.setValue("buttonHeight", String.valueOf(_goToTopConfiguration.buttonHeight()));
			preferences.setValue("buttonHAlignment", String.valueOf(_goToTopConfiguration.buttonHAlignment()));
			preferences.setValue("buttonHPosition", String.valueOf(_goToTopConfiguration.buttonHPosition()));
			preferences.setValue("buttonVAlignment", String.valueOf(_goToTopConfiguration.buttonVAlignment()));
			preferences.setValue("buttonVPosition", String.valueOf(_goToTopConfiguration.buttonVPosition()));
			preferences.setValue("scrollSpeed", String.valueOf(_goToTopConfiguration.scrollSpeed()));
			preferences.setValue("appearencePosition", String.valueOf(_goToTopConfiguration.appearencePosition()));
			preferences.setValue("transitionTime", String.valueOf(_goToTopConfiguration.transitionTime()));
			preferences.setValue("textButtonSpace", String.valueOf(_goToTopConfiguration.textButtonSpace()));
			try {
				preferences.store();
			} catch (Exception e) {
				_log.error(e);
			}
		}
		// -----

		GoToTopConfiguration goToTopConfiguration = _configurationProvider.getPortletInstanceConfiguration(
				GoToTopConfiguration.class, themeDisplay.getLayout(), portletId);
		
		httpServletRequest.setAttribute(GoToTopConfiguration.class.getName(), goToTopConfiguration);
		httpServletRequest.setAttribute(ItemSelector.class.getName(), _itemSelector);
		httpServletRequest.setAttribute(DLAppLocalService.class.getName(), _dlAppLocalService);
		httpServletRequest.setAttribute(DLURLHelper.class.getName(), _dlURLHelper);
		httpServletRequest.setAttribute(Configuration.class.getName(), _configuration);
		httpServletRequest.setAttribute(PortletLocalService.class.getName(), _portletLocalService);
		
		super.include(portletConfig, httpServletRequest, httpServletResponse);
	}
	
	@Activate
	@Modified
	protected void activate(Map<Object, Object> properties) {
		_configuration = ConfigurationFactoryUtil.getConfiguration(PortalClassLoaderUtil.getClassLoader(), "portlet");
		_goToTopConfiguration = ConfigurableUtil.createConfigurable(GoToTopConfiguration.class, properties);
		_validators = new Validators(_configuration, _dlAppLocalService);
		
	}
	
	@Override
	@Reference(
		target = "(osgi.web.symbolicname=ru.hitrome.java.liferay.gototop)", unbind = "-"
	)
	public void setServletContext(ServletContext servletContext) {
		super.setServletContext(servletContext);
	}
	
	@Reference(unbind = "-")
	protected void setDLAppLocalService(DLAppLocalService dlAppLocalService) {
		_dlAppLocalService = dlAppLocalService;
	}
	
	@Reference(unbind = "-")
	protected void setPortletLocalService(PortletLocalService portletLocalService) {
		_portletLocalService = portletLocalService;
	}
	
	@Reference(unbind = "-")
	protected void setDLURLHelper(DLURLHelper dlURLHelper) {
		_dlURLHelper = dlURLHelper;
	}
	
	@Reference
	protected void setConfigurationProvider(ConfigurationProvider configurationProvider) {
	    _configurationProvider = configurationProvider;
	}
	
	
	private Configuration _configuration;
	private ConfigurationProvider _configurationProvider;
	
	@Reference
	private ItemSelector _itemSelector;
	
	private DLAppLocalService _dlAppLocalService;
	private DLURLHelper _dlURLHelper;
	private PortletLocalService _portletLocalService;
	
	private volatile GoToTopConfiguration _goToTopConfiguration;
	
	
	private Validators _validators;
	
	private static final Log _log = LogFactoryUtil.getLog(GoToTopConfigurationAction.class);

}
