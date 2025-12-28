package com.app.backend.responses.chat;

import com.app.backend.models.TinNhanPhongChat;
import com.app.backend.models.enums.LoaiTinNhan;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.Instant;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TinNhanResponse {

    private Long id;
    private Long phongChatId;
    private NguoiGuiResponse nguoiGui;
    private LoaiTinNhan loai;
    private String noiDung;
    private String urlMedia;
    private String tenFile;
    private Long kichThuocFile;
    private TinNhanTraLoiResponse traLoiCho;
    private Instant guiLuc;
    private Instant chinhSuaLuc;
    private Boolean daGhim;
    private Boolean laToi; // Có phải tin nhắn của current user không

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class NguoiGuiResponse {
        private Long id;
        private String ten;
        private String anhDaiDien;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class TinNhanTraLoiResponse {
        private Long id;
        private String noiDung;
        private String tenNguoiGui;
    }

    public static TinNhanResponse fromEntity(TinNhanPhongChat entity, Long currentUserId) {
        if (entity == null) return null;

        TinNhanResponseBuilder builder = TinNhanResponse.builder()
                .id(entity.getId())
                .phongChatId(entity.getPhongChat().getId())
                .loai(entity.getLoai())
                .noiDung(entity.getNoiDung())
                .urlMedia(entity.getUrlMedia())
                .tenFile(entity.getTenFile())
                .kichThuocFile(entity.getKichThuocFile())
                .guiLuc(entity.getGuiLuc())
                .chinhSuaLuc(entity.getChinhSuaLuc())
                .daGhim(entity.getDaGhim())
                .laToi(entity.getGuiBoi().getId().equals(currentUserId));

        // Người gửi
        if (entity.getGuiBoi() != null) {
            builder.nguoiGui(NguoiGuiResponse.builder()
                    .id(entity.getGuiBoi().getId())
                    .ten(entity.getGuiBoi().getHoTen())
                    .anhDaiDien(entity.getGuiBoi().getAvatarUrl())
                    .build());
        }

        // Tin nhắn đang trả lời
        if (entity.getTraLoiCho() != null) {
            builder.traLoiCho(TinNhanTraLoiResponse.builder()
                    .id(entity.getTraLoiCho().getId())
                    .noiDung(truncateText(entity.getTraLoiCho().getNoiDung(), 100))
                    .tenNguoiGui(entity.getTraLoiCho().getGuiBoi() != null ?
                            entity.getTraLoiCho().getGuiBoi().getHoTen() : null)
                    .build());
        }

        return builder.build();
    }

    private static String truncateText(String text, int maxLength) {
        if (text == null) return null;
        if (text.length() <= maxLength) return text;
        return text.substring(0, maxLength) + "...";
    }
}
