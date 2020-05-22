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
package ru.hitrome.java.liferay.gototop.portlet;

import ru.hitrome.java.liferay.gototop.constants.GototopPortletKeys;
import ru.hitrome.java.liferay.gototop.portlet.config.GoToTopConfiguration;

import com.liferay.document.library.kernel.service.DLAppLocalService;
import com.liferay.document.library.util.DLURLHelper;
import com.liferay.portal.kernel.log.Log;
import com.liferay.portal.kernel.log.LogFactoryUtil;
import com.liferay.portal.kernel.module.configuration.ConfigurationException;
import com.liferay.portal.kernel.module.configuration.ConfigurationProvider;
import com.liferay.portal.kernel.portlet.bridges.mvc.MVCPortlet;
import com.liferay.portal.kernel.theme.ThemeDisplay;
import com.liferay.portal.kernel.util.PortalUtil;
import com.liferay.portal.kernel.util.WebKeys;

import java.io.IOException;
import javax.portlet.Portlet;
import javax.portlet.PortletException;
import javax.portlet.RenderRequest;
import javax.portlet.RenderResponse;

import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;

/**
 * Main MVC controller class.
 * 
 * @author Roman Novikov (rrl-software@mail.ru, http://hitrome.ru)
 *
 */
@Component(
	configurationPid = "ru.hitrome.java.liferay.gototop.portlet.config.GoToTopConfiguration",
	immediate = true,
	property = {
		"com.liferay.portlet.display-category=category.tools",
		"com.liferay.portlet.header-portlet-css=/css/main.css",
		"com.liferay.portlet.instanceable=true",
		"javax.portlet.display-name=Gototop",
		"javax.portlet.init-param.template-path=/",
		"javax.portlet.init-param.config-template=/configuration.jsp",
		"javax.portlet.init-param.view-template=/view.jsp",
		"javax.portlet.name=" + GototopPortletKeys.GOTOTOP,
		"javax.portlet.resource-bundle=content.Language",
		"javax.portlet.security-role-ref=power-user,user"
	},
	service = Portlet.class
)
public class GototopPortlet extends MVCPortlet {
	
	
	@Override
	public void doView(RenderRequest renderRequest, RenderResponse renderResponse) throws IOException, PortletException {
		
		ThemeDisplay themeDisplay = (ThemeDisplay) renderRequest.getAttribute(WebKeys.THEME_DISPLAY);
		GoToTopConfiguration goToTopConfiguration = null;
		try {
			goToTopConfiguration = _configurationProvider.getPortletInstanceConfiguration(
					GoToTopConfiguration.class, themeDisplay.getLayout(), PortalUtil.getPortletId(renderRequest));
		} catch (ConfigurationException e) {
			_log.fatal(e);
		}
		
		renderRequest.setAttribute(GoToTopConfiguration.class.getName(), goToTopConfiguration);
		renderRequest.setAttribute(DLAppLocalService.class.getName(), _dlAppLocalService);
		renderRequest.setAttribute(DLURLHelper.class.getName(), _dlURLHelper);
		
		super.doView(renderRequest, renderResponse);
	}
	
	@Reference
	protected void setConfigurationProvider(ConfigurationProvider configurationProvider) {
	    _configurationProvider = configurationProvider;
	}
	
	@Reference(unbind = "-")
	protected void setDLAppLocalService(DLAppLocalService dlAppLocalService) {
		_dlAppLocalService = dlAppLocalService;
	}
	
	@Reference(unbind = "-")
	protected void setDLURLHelper(DLURLHelper dlURLHelper) {
		_dlURLHelper = dlURLHelper;
	}
	
	private ConfigurationProvider _configurationProvider;
	private DLAppLocalService _dlAppLocalService;
	private DLURLHelper _dlURLHelper;
	
	private static final Log _log = LogFactoryUtil.getLog(GototopPortlet.class);
}
