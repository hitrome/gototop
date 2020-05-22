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

<%@ page import="com.liferay.portal.kernel.util.Constants" %>
<%@ page import="com.liferay.portal.kernel.portlet.RequestBackedPortletURLFactory" %>
<%@ page import="com.liferay.portal.kernel.portlet.RequestBackedPortletURLFactoryUtil" %>
<%@ page import="com.liferay.item.selector.ItemSelectorReturnType" %>
<%@ page import="com.liferay.item.selector.criteria.FileEntryItemSelectorReturnType" %>
<%@ page import="com.liferay.item.selector.criteria.image.criterion.ImageItemSelectorCriterion" %>
<%@ page import="com.liferay.item.selector.ItemSelector" %>
<%@ page import="com.liferay.portal.kernel.util.ParamUtil" %>

<%@ page import="ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ImagePathException" %>
<%@ page import="ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ImageIdException" %>
<%@ page import="ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ColorTextException" %>
<%@ page import="ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonTextException" %>
<%@ page import="ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonTextFontFamilyException" %>
<%@ page import="ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonTextFontSizeException" %>
<%@ page import="ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonTextFontStyleBoldException" %>
<%@ page import="ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonTextFontStyleItalicException" %>
<%@ page import="ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonTextFontStyleUnderlineException" %>
<%@ page import="ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonTextDecorationCapitalizeException" %>
<%@ page import="ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonWidthException" %>
<%@ page import="ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonHeightException" %>
<%@ page import="ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonHAlignmentException" %>
<%@ page import="ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonHPositionException" %>
<%@ page import="ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonVAlignmentException" %>
<%@ page import="ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ButtonVPositionException" %>
<%@ page import="ru.hitrome.java.liferay.gototop.portlet.config.exceptions.ScrollSpeedException" %>
<%@ page import="ru.hitrome.java.liferay.gototop.portlet.config.exceptions.AppearencePositionException" %>
<%@ page import="ru.hitrome.java.liferay.gototop.portlet.config.exceptions.TransitionTimeException" %>
<%@ page import="ru.hitrome.java.liferay.gototop.portlet.config.exceptions.TextButtonSpaceException" %>

<div class="gototop-hitrome-config-messages">
	<liferay-ui:error exception="<%= ImagePathException.class %>" message="gototop-error-image-path-long-or-not-correct" />
	<liferay-ui:error exception="<%= ImageIdException.class %>" message="gototop-error-wrong-image-id-or-no-such-an-image" />
	<liferay-ui:error exception="<%= ColorTextException.class %>" message="gototop-error-wrong-color-text" />
	<liferay-ui:error exception="<%= ButtonTextException.class %>" message="gototop-error-button-text-too-long" />
	<liferay-ui:error exception="<%= ButtonTextFontFamilyException.class %>" message="gototop-error-wrong-button-text-font-family-value" />
	<liferay-ui:error exception="<%= ButtonTextFontSizeException.class %>" message="gototop-error-wrong-font-size" />
	<liferay-ui:error exception="<%= ButtonTextFontStyleBoldException.class %>" message="gototop-error-wrong-button-text-style-bold" />
	<liferay-ui:error exception="<%= ButtonTextFontStyleItalicException.class %>" message="gototop-error-wrong-button-text-style-italic" />
	<liferay-ui:error exception="<%= ButtonTextFontStyleUnderlineException.class %>" message="gototop-error-wrong-button-text-style-underline" />
	<liferay-ui:error exception="<%= ButtonTextDecorationCapitalizeException.class %>" message="gototop-error-wrong-button-text-decoration-uppercase" />
	<liferay-ui:error exception="<%= ButtonWidthException.class %>" message="gototop-error-wrong-button-width" />
	<liferay-ui:error exception="<%= ButtonHeightException.class %>" message="gototop-error-wrong-button-height" />
	<liferay-ui:error exception="<%= ButtonHAlignmentException.class %>" message="gototop-error-wrong-horizontal-alignment-value" />
	<liferay-ui:error exception="<%= ButtonHPositionException.class %>" message="gototop-error-wrong-horizontal-position-value" />
	<liferay-ui:error exception="<%= ButtonVAlignmentException.class %>" message="gototop-error-wrong-vertical-alignment-value" />
	<liferay-ui:error exception="<%= ButtonVPositionException.class %>" message="gototop-error-wrong-vertical-position-value" />
	<liferay-ui:error exception="<%= ScrollSpeedException.class %>" message="gototop-error-wrong-scroll-speed" />
	<liferay-ui:error exception="<%= AppearencePositionException.class %>" message="gototop-error-wrong-appearence-position" />
	<liferay-ui:error exception="<%= TransitionTimeException.class %>" message="gototop-error-wrong-appearence-time" />
	<liferay-ui:error exception="<%= TextButtonSpaceException.class %>" message="gototop-error-wrong-text-button-space-value" />
</div>

<c:choose>
<c:when test="<%= Validator.isNotNull(goToTopConfiguration) && Validator.isNotNull(configuration) %>">

<%
PortletURL itemSelectorPortletURL = null;
ItemSelector itemSelector = (ItemSelector) renderRequest.getAttribute(ItemSelector.class.getName());
if (Validator.isNotNull(itemSelector)) {
	RequestBackedPortletURLFactory requestBackedPortletURLFactory = 
		RequestBackedPortletURLFactoryUtil.create(request);
	List<ItemSelectorReturnType> itemSelectorReturnTypes = new ArrayList<>();
	itemSelectorReturnTypes.add(new FileEntryItemSelectorReturnType());
	ImageItemSelectorCriterion imageItemSelectorCriterion = new ImageItemSelectorCriterion();
	imageItemSelectorCriterion.setDesiredItemSelectorReturnTypes(itemSelectorReturnTypes);
	itemSelectorPortletURL = itemSelector.getItemSelectorURL(requestBackedPortletURLFactory, "goToTopISEvent",
			imageItemSelectorCriterion);
}
String portletId = ParamUtil.getString(request, "portletResource");
String imagePreview = portletLocalService.getPortletById(portletId).getContextPath() + "/images/noimage.png";
try {
	FileEntry fileEntry = dlAppLocalService.getFileEntry(goToTopConfiguration.imageId());
	imagePreview = dlURLHelper.getImagePreviewURL(fileEntry, themeDisplay);
} catch (PortalException e) {
	// System.out.println(e.getMessage());
}
String maxStringLengthMessage = LanguageUtil.get(request, "gototop-max-string-length") + StringPool.COLON + StringPool.SPACE;
String maxColorStringLengthMessage = LanguageUtil.get(request, "gototop-max-color-string-length") + StringPool.COLON + StringPool.SPACE;
String configImagePathMaxLength = configuration.get(GoToTopConstants.MAX_CONFIG_IMAGE_PATH);
String configColorTextMaxLength = configuration.get(GoToTopConstants.MAX_CONFIG_COLOR_TEXT_LENGTH);
String configButtonTextMaxLength = configuration.get(GoToTopConstants.MAX_CONFIG_BUTTON_TEXT_LENGTH);
String configTextButtonSpace = configuration.get(GoToTopConstants.MAX_CONFIG_TEXT_BUTTON_SPACE);
%>

<liferay-portlet:actionURL portletConfiguration="<%= true %>" var="configurationActionURL" />

<liferay-portlet:renderURL portletConfiguration="<%= true %>" var="configurationRenderURL" />



<liferay-frontend:edit-form
	action="<%= configurationActionURL %>"
	method="post"
	name="fm"
>
<aui:input name="<%= Constants.CMD %>" type="hidden" value="<%= Constants.UPDATE %>" />

<aui:input name="redirect" type="hidden" value="<%= configurationRenderURL %>" />

<aui:fieldset>
	<aui:input name="<%= GoToTopConstants.CONFIG_IMAGE_PATH_KEY %>" value="<%= goToTopConfiguration.imagePath() %>" placeholder="<%= maxStringLengthMessage + configImagePathMaxLength %>" helpMessage="config-image-path-help-message" />
	<div class="d-flex">
		<aui:input name="<%= GoToTopConstants.CONFIG_IMAGE_ID_KEY %>" type="number" value="<%= goToTopConfiguration.imageId() %>" min="0" max="<%= Long.MAX_VALUE %>" helpMessage="config-image-id-help-message" />
		<div style="float:left;margin-left:10px;">
			<label style="display:block;">&nbsp;</label>
			<aui:button name="config-select-image" value="select" />
		</div>
	</div>
	<div class="gototop-hitrome-config-preview">
		<img src="<%= imagePreview %>" height="300" id="<portlet:namespace />preview" />
	</div>
	<div class="d-flex gototop-hitrome-config-group">
		<aui:input name="<%= GoToTopConstants.CONFIG_COLOR_TEXT_KEY %>" value="<%= goToTopConfiguration.colorText() %>" placeholder="<%= maxColorStringLengthMessage + configColorTextMaxLength %>" helpMessage="config-color-text-help-message" />
		<%
		String colorPreviewStyle = "";
		if (!goToTopConfiguration.colorText().isEmpty()) {
			colorPreviewStyle = "style=\"background:" + goToTopConfiguration.colorText() + "\" ";
		}
		%>
		<div style="float:left;">
			<label style="display:block;" class="control-label">&nbsp;</label>
			<div <%= colorPreviewStyle %>class="gototop-hitrome-config-btn-color" id="<portlet:namespace />config-btn-color">
				
			</div>
		</div>
		<div style="float:left;">
			<label style="display:block;">&nbsp;</label>
			<aui:button name="config-select-color" value="select" />
		</div>
	</div>
	<div class="gototop-hitrome-config-group gototop-hitrome-config-bottom-space">
		<aui:field-wrapper label="config-button-text">
			<liferay-ui:input-localized name="<%= GoToTopConstants.CONFIG_BUTTON_TEXT_KEY %>" xml="<%= goToTopConfiguration.buttonText() %>" placeholder="<%= maxStringLengthMessage + configButtonTextMaxLength %>" />
		</aui:field-wrapper>
		<div class="container">
			<div class="row">
				<div class="col gototop-hitrome-config-columns">
					<aui:select multiple="false" name="<%= GoToTopConstants.CONFIG_BUTTON_TEXT_FONT_FAMILY_KEY %>">
						<%
						for (int i = 0; i < GoToTopConstants.FONT_FAMILIES.length; i++) {
						%>
						<aui:option label='<%= GoToTopConstants.FONT_FAMILIES[i].replaceAll(StringPool.QUOTE, "") %>' selected="<%= GoToTopConstants.FONT_FAMILIES[i].equals(goToTopConfiguration.buttonTextFontFamily()) %>" style='<%= "font-family:" + GoToTopConstants.FONT_FAMILIES[i] + StringPool.SEMICOLON %>' value="<%= String.valueOf(i) %>" />
						<%
						}
						%>
					</aui:select>
				</div>
				<div class="col gototop-hitrome-config-columns">
					<aui:input name="<%= GoToTopConstants.CONFIG_BUTTON_TEXT_FONT_SIZE_KEY %>" type="number" value="<%= goToTopConfiguration.buttonTextFontSize() %>" min="0" max="<%= configuration.get(GoToTopConstants.MAX_CONFIG_BUTTON_TEXT_FONT_SIZE) %>" />
				</div>
			</div>
		</div>
		<aui:fieldset label="config-button-text-style-decoration">
			<div class="container">
				<div class="row">
					<div class="col gototop-hitrome-config-columns">
						<aui:input name="config-button-text-font-style-bold" type="checkbox" checked="<%= goToTopConfiguration.buttonTextFontStyleBold() %>"/>
					</div>
					<div class="col gototop-hitrome-config-columns">
						<aui:input name="config-button-text-font-style-italic" type="checkbox" checked="<%= goToTopConfiguration.buttonTextFontStyleItalic() %>"/>
					</div>
				</div>
				<div class="row">
					<div class="col gototop-hitrome-config-columns">
						<aui:input name="config-button-text-font-style-underline" type="checkbox" checked="<%= goToTopConfiguration.buttonTextFontStyleUnderline() %>"/>
					</div>
					<div class="col gototop-hitrome-config-columns">
						<aui:input name="config-button-text-decoration-capitalize" type="checkbox" checked="<%= goToTopConfiguration.buttonTextDecorationCapitalize() %>"/>
					</div>
				</div>
			</div>
		</aui:fieldset>
	</div>
	<div class="container gototop-hitrome-config-group">
		<div class="row">
			<div class="col gototop-hitrome-config-columns">
				<aui:input name="<%= GoToTopConstants.CONFIG_BUTTON_WIDTH_KEY %>" type="number" value="<%= goToTopConfiguration.buttonWidth() %>" min="0" max="<%= configuration.get(GoToTopConstants.MAX_CONFIG_BUTTON_WIDTH) %>" />
			</div>
			<div class="col gototop-hitrome-config-columns">
				<aui:input name="<%= GoToTopConstants.CONFIG_BUTTON_HEIGHT_KEY %>" type="number" value="<%= goToTopConfiguration.buttonHeight() %>" min="0" max="<%= configuration.get(GoToTopConstants.MAX_CONFIG_BUTTON_HEIGHT) %>" />
			</div>
		</div>
		<div class="row gototop-hitrome-config-group">
			<div class="col gototop-hitrome-config-columns">
				<aui:select multiple="false" name="<%= GoToTopConstants.CONFIG_BUTTON_H_ALIGNMENT_KEY %>">
					<aui:option label="config-h-alignment-right" selected="<%= goToTopConfiguration.buttonHAlignment() == false %>" value="<%= Boolean.FALSE.toString() %>" />
					<aui:option label="config-h-alignment-left" selected="<%= goToTopConfiguration.buttonHAlignment() == true %>" value="<%= Boolean.TRUE.toString() %>" />
				</aui:select>
			</div>
			<div class="col gototop-hitrome-config-columns">
				<aui:input name="<%= GoToTopConstants.CONFIG_BUTTON_H_POSITION_KEY %>" type="number" value="<%= goToTopConfiguration.buttonHPosition() %>" min="0" max="<%= configuration.get(GoToTopConstants.MAX_CONFIG_BUTTON_H_POSITION) %>" />
			</div>
		</div>
		<div class="row">
			<div class="col gototop-hitrome-config-columns">
				<aui:select multiple="false" name="<%= GoToTopConstants.CONFIG_BUTTON_V_ALIGNMENT_KEY %>">
					<aui:option label="config-v-alignment-bottom" selected="<%= goToTopConfiguration.buttonVAlignment() == false %>" value="<%= Boolean.FALSE.toString() %>" />
					<aui:option label="config-v-alignment-top" selected="<%= goToTopConfiguration.buttonVAlignment() == true %>" value="<%= Boolean.TRUE.toString() %>" />
				</aui:select>
			</div>
			<div class="col gototop-hitrome-config-columns">
				<aui:input name="<%= GoToTopConstants.CONFIG_BUTTON_V_POSITION_KEY %>" type="number" value="<%= goToTopConfiguration.buttonVPosition() %>" min="0" max="<%= configuration.get(GoToTopConstants.MAX_CONFIG_BUTTON_V_POSITION) %>" />
			</div>
		</div>
		<div class="row gototop-hitrome-config-group">
			<div class="col gototop-hitrome-config-columns">
				<aui:input name="<%= GoToTopConstants.CONFIG_BUTTON_SCROLL_SPEED_KEY %>" type="number" value="<%= goToTopConfiguration.scrollSpeed() %>" min="0" max="<%= configuration.get(GoToTopConstants.MAX_CONFIG_SCROLL_SPEED) %>" />
			</div>
			<div class="col gototop-hitrome-config-columns">
				<aui:input name="<%= GoToTopConstants.CONFIG_BUTTON_APPEARENCE_POSITION_KEY %>" type="number" value="<%= goToTopConfiguration.appearencePosition() %>" min="0" max="<%= configuration.get(GoToTopConstants.MAX_CONFIG_APPEARENCE_POSITION) %>" />
			</div>
		</div>
		<div class="row">
			<div class="col gototop-hitrome-config-columns">
				<aui:input name="<%= GoToTopConstants.CONFIG_BUTTON_TRANSITION_TIME_KEY %>" type="number" value="<%= goToTopConfiguration.transitionTime() %>" min="0" max="<%= configuration.get(GoToTopConstants.MAX_CONFIG_TRANSITION_TIME) %>" />
			</div>
			<div class="col gototop-hitrome-config-columns">
				<aui:input name="<%= GoToTopConstants.CONFIG_BUTTON_TEXTBUTTON_SPACE_KEY %>" type="number" value="<%= goToTopConfiguration.textButtonSpace() %>" min='<%= "-" + configTextButtonSpace %>' max="<%= configTextButtonSpace %>"/>
			</div>
		</div>
	</div>
</aui:fieldset>

<aui:button-row>
	<aui:button type="submit" />
	<aui:button type="cancel" />
</aui:button-row>

</liferay-frontend:edit-form>

<aui:script use="liferay-item-selector-dialog">
	$("#<portlet:namespace/>config-select-image").on(
		'click',
		function(event) {
			var itemSelectorDialog = new A.LiferayItemSelectorDialog(
				{
					eventName: 'goToTopISEvent',
					on: {
						selectedItemChange: function(event) {
							var selectedItem = event.newVal;
							if (selectedItem) {
								var itemValue = JSON.parse(selectedItem.value);
								fileEntryId = itemValue.fileEntryId;
								url = itemValue.url;
								$('#<portlet:namespace/>config-image-id').val(fileEntryId);
								$('#<portlet:namespace/>preview').attr('src', url);
							}
						}
					},
					title: '<liferay-ui:message key="select-image" />',
					url: '<%= itemSelectorPortletURL.toString() %>'
				}
			);
			itemSelectorDialog.open();
		});
</aui:script>
<aui:script use="aui-color-picker-popover">
	
	var colorSelectorDialog = new A.ColorPickerPopover(
		{
			trigger: '#<portlet:namespace/>config-select-color',
			zIndex: 2,
			strings: {
            		cancel: '<liferay-ui:message key="gototop-cp-cancel" />',
            		more: '<liferay-ui:message key="gototop-cp-more-colors" />',
            		noColor: 'No color',
            		none: '<liferay-ui:message key="gototop-cp-none" />',
            		ok: 'OK'
    		}
		}
	).render();
	colorSelectorDialog.on(
		'select',
		function(event) {
			$("#<portlet:namespace/>config-color-text").val(event.color);
			$("#<portlet:namespace/>config-btn-color").css('background', event.color);
		}
	);
</aui:script>

</c:when>
<c:otherwise>

</c:otherwise>
</c:choose>