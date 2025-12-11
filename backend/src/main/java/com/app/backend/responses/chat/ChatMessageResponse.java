package com.app.backend.responses.chat;

import com.app.backend.models.TinNhan;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;

import java.time.Instant;

@Builder
public record ChatMessageResponse(
        @JsonProperty("tin_nhan_id")
        Long tinNhanId,
        @JsonProperty("gui_boi_id")
        Long guiBoiId,
        @JsonProperty("gui_boi_ten")
        String guiBoiTen,
        @JsonProperty("nhan_boi_id")
        Long nhanBoiId,
        @JsonProperty("nhan_boi_ten")
        String nhanBoiTen,
        @JsonProperty("noi_dung")
        String noiDung,
        @JsonProperty("gui_luc")
        Instant guiLuc,
        @JsonProperty("la_toi")
        boolean laToi
) {
    public static ChatMessageResponse fromEntity(TinNhan e, Long currentUserId) {
        boolean isMe = currentUserId != null
                && e.getGuiBoi() != null
                && currentUserId.equals(e.getGuiBoi().getId());

        return ChatMessageResponse.builder()
                .tinNhanId(e.getId())
                .guiBoiId(e.getGuiBoi() != null ? e.getGuiBoi().getId() : null)
                .guiBoiTen(e.getGuiBoi() != null ? e.getGuiBoi().getHoTen() : null)
                .nhanBoiId(e.getNhanBoi() != null ? e.getNhanBoi().getId() : null)
                .nhanBoiTen(e.getNhanBoi() != null ? e.getNhanBoi().getHoTen() : null)
                .noiDung(e.getNoiDung())
                .guiLuc(e.getGuiLuc())
                .laToi(isMe)
                .build();
    }

    /**
     * Dùng cho chỗ nào chưa cần `la_toi` (ví dụ WebSocket cũ),
     * mặc định la_toi = false.
     */
    public static ChatMessageResponse fromEntity(TinNhan e) {
        return fromEntity(e, null);
    }
}

