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

<%@ include file="./init.jsp" %>
<%@ page import="com.liferay.portal.kernel.util.JavaConstants" %>
<%@ page import="javax.portlet.PortletResponse" %>

<c:if test="<%= Validator.isNotNull(goToTopConfiguration) %>">
<%
String imageURL = "";
if (goToTopConfiguration.imagePath().isEmpty()) {
	try {
		FileEntry fileEntry = dlAppLocalService.getFileEntry(goToTopConfiguration.imageId());
		imageURL = dlURLHelper.getImagePreviewURL(fileEntry, themeDisplay);
		
	} catch (PortalException e) {
		// Do nothing
	}
} else {
	imageURL = goToTopConfiguration.imagePath().startsWith(GoToTopConstants.CONTEXT_PATH_SHORTCUT) ? request.getContextPath()
			+ goToTopConfiguration.imagePath().substring(GoToTopConstants.CONTEXT_PATH_SHORTCUT.length(), goToTopConfiguration.imagePath().length())
			: goToTopConfiguration.imagePath();
	
}

// Image
StringBuilder goToTopImageStyle = new StringBuilder();
goToTopImageStyle.append("width:");
goToTopImageStyle.append(goToTopConfiguration.buttonWidth());
goToTopImageStyle.append("px;");
goToTopImageStyle.append("height:");
goToTopImageStyle.append(goToTopConfiguration.buttonHeight());
goToTopImageStyle.append("px;");
goToTopImageStyle.append("background: url(");
goToTopImageStyle.append(imageURL);
goToTopImageStyle.append(") no-repeat;");
goToTopImageStyle.append("-moz-background-size: 100%;");
goToTopImageStyle.append("-webkit-background-size: 100%;");
goToTopImageStyle.append("-o-background-size: 100%;");
goToTopImageStyle.append("background-size: 100%;");
goToTopImageStyle.append("margin-bottom:");
goToTopImageStyle.append(goToTopConfiguration.textButtonSpace());
goToTopImageStyle.append("px;");

// Text
StringBuilder goToTopTextStyle = new StringBuilder();
goToTopTextStyle.append("width:");
goToTopTextStyle.append(goToTopConfiguration.buttonWidth());
goToTopTextStyle.append("px;");
if (!goToTopConfiguration.colorText().isEmpty()) {
	goToTopTextStyle.append("color:");
	goToTopTextStyle.append(goToTopConfiguration.colorText());
	goToTopTextStyle.append(";");
}
if (!goToTopConfiguration.buttonTextFontFamily().isEmpty()) {
	goToTopTextStyle.append("font-family:");
	goToTopTextStyle.append(goToTopConfiguration.buttonTextFontFamily());
	goToTopTextStyle.append(";");
}
goToTopTextStyle.append("font-size:");
goToTopTextStyle.append(goToTopConfiguration.buttonTextFontSize());
goToTopTextStyle.append("pt;");
goToTopTextStyle.append("font-weight:");
goToTopTextStyle.append(goToTopConfiguration.buttonTextFontStyleBold() ? "bold;" : "normal;");
goToTopTextStyle.append("font-style:");
goToTopTextStyle.append(goToTopConfiguration.buttonTextFontStyleItalic() ? "italic;" : "normal;");
goToTopTextStyle.append("text-decoration:");
goToTopTextStyle.append(goToTopConfiguration.buttonTextFontStyleUnderline() ? "underline;" : "none;");
goToTopTextStyle.append("text-transform:");
goToTopTextStyle.append(goToTopConfiguration.buttonTextDecorationCapitalize() ? "uppercase;" : "none;");

// Holder
StringBuilder goToTopHolder = new StringBuilder();
if (goToTopConfiguration.buttonVAlignment()) {
	goToTopHolder.append("top:");
} else {
	goToTopHolder.append("bottom:");
}
goToTopHolder.append(goToTopConfiguration.buttonVPosition());
goToTopHolder.append("px;");
if (goToTopConfiguration.buttonHAlignment()) {
	goToTopHolder.append("left:");
} else {
	goToTopHolder.append("right:");
}
goToTopHolder.append(goToTopConfiguration.buttonHPosition());
goToTopHolder.append("px;");
// --

String buttonText = LocalizationUtil.getLocalization(goToTopConfiguration.buttonText(), themeDisplay.getLanguageId(), true);
PortletResponse portletResponse = (PortletResponse)request.getAttribute(JavaConstants.JAVAX_PORTLET_RESPONSE);
%>

<c:if test="<%= Validator.isNotNull(portletResponse) %>">
<style>
#portlet<%= portletResponse.getNamespace().substring(0, portletResponse.getNamespace().length() - 1) %> .portlet-header {
	display: none;
}

</style>
</c:if>


<div id="<portlet:namespace />back-to-top-button" class="gototop-hitrome-holder" style="<%= goToTopHolder %>">
<c:if test="<%= !imageURL.isEmpty() %>">
	<div class="gototop-hitrome-image" style="<%= goToTopImageStyle.toString() %>">
	</div>
</c:if>
<c:if test="<%= !buttonText.isEmpty() %>">	
	<div class="gototop-hitrome-text" style="<%= goToTopTextStyle.toString() %>">
		<%= buttonText %>
	</div>
</c:if>
</div>
<aui:script>
	function goToTop() {
		$('body,html').animate(
			{
				scrollTop: 0
			},
			<%= goToTopConfiguration.scrollSpeed() %>,
			'swing'
		);
		return false;
	}

	$('#<portlet:namespace />back-to-top-button').hide();
	$(window).scroll(
		function() {
			if ($(this).scrollTop() > <%= goToTopConfiguration.appearencePosition() %>) {
				$('#<portlet:namespace />back-to-top-button').fadeIn(<%= goToTopConfiguration.transitionTime() %>);
			} else {
				$('#<portlet:namespace />back-to-top-button').fadeOut(<%= goToTopConfiguration.transitionTime() %>);
			}
		}
	);
<c:if test="<%= !imageURL.isEmpty() %>">
	$('#<portlet:namespace />back-to-top-button .gototop-hitrome-image').click(
		function() {
			goToTop();
		}
	);
</c:if>
<c:if test="<%= !buttonText.isEmpty() %>">
	$('#<portlet:namespace />back-to-top-button .gototop-hitrome-text').click(
		function() {
			goToTop();
		}
	);
</c:if>
</aui:script>
</c:if>

