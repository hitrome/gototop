<%--
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
 --%>

<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>

<%@ taglib uri="http://java.sun.com/portlet_2_0" prefix="portlet" %>

<%@ taglib uri="http://liferay.com/tld/aui" prefix="aui" %>
<%@ taglib uri="http://liferay.com/tld/portlet" prefix="liferay-portlet" %>
<%@ taglib uri="http://liferay.com/tld/frontend" prefix="liferay-frontend" %>
<%@ taglib uri="http://liferay.com/tld/theme" prefix="liferay-theme" %>
<%@ taglib uri="http://liferay.com/tld/ui" prefix="liferay-ui" %>
<%@ taglib uri="http://liferay.com/tld/clay" prefix="clay"%>

<%@ page import="java.util.List" %>
<%@ page import="java.util.ArrayList" %>
<%@ page import="javax.portlet.PortletURL" %>

<%@ page import="com.liferay.petra.string.StringPool" %>
<%@ page import="com.liferay.portal.kernel.util.Validator" %>
<%@ page import="com.liferay.document.library.util.DLURLHelper" %>
<%@ page import="com.liferay.document.library.kernel.service.DLAppLocalService" %>
<%@ page import="com.liferay.portal.kernel.service.PortletLocalService" %>
<%@ page import="com.liferay.portal.kernel.repository.model.FileEntry" %>
<%@ page import="com.liferay.portal.kernel.util.LocalizationUtil" %>
<%@ page import="com.liferay.portal.kernel.language.LanguageUtil" %>
<%@ page import="com.liferay.portal.kernel.configuration.Configuration" %>

<%@ page import="ru.hitrome.java.liferay.gototop.portlet.config.GoToTopConfiguration" %>
<%@ page import="ru.hitrome.java.liferay.gototop.constants.GoToTopConstants" %>

<%@ page import="com.liferay.portal.kernel.exception.PortalException" %>

<liferay-theme:defineObjects />

<portlet:defineObjects />

<%
Configuration configuration = (Configuration) renderRequest.getAttribute(Configuration.class.getName());
GoToTopConfiguration goToTopConfiguration
	= (GoToTopConfiguration)renderRequest.getAttribute(GoToTopConfiguration.class.getName());
DLAppLocalService dlAppLocalService = (DLAppLocalService) renderRequest.getAttribute(DLAppLocalService.class.getName());
DLURLHelper dlURLHelper = (DLURLHelper) renderRequest.getAttribute(DLURLHelper.class.getName());
PortletLocalService portletLocalService = (PortletLocalService) renderRequest.getAttribute(PortletLocalService.class.getName());
%>