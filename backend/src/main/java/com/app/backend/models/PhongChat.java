package com.app.backend.models;

import com.app.backend.models.enums.LoaiPhongChat;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonManagedReference;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

@Entity
@Table(name = "phong_chat")
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PhongChat {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "ten", length = 100)
    private String ten; // Tên phòng (cho group chat)

    @Column(name = "anh_nhom")
    private String anhNhom; // Ảnh đại diện nhóm

    @Enumerated(EnumType.STRING)
    @Column(name = "loai", nullable = false)
    @Builder.Default
    private LoaiPhongChat loai = LoaiPhongChat.DON; // DON = 1-1, NHOM = group

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "tao_boi_id")
    private NguoiDung taoBoi;

    @Column(name = "tao_luc", updatable = false)
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant taoLuc;

    @Column(name = "cap_nhat_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant capNhatLuc;

    @Column(name = "tin_nhan_cuoi")
    private String tinNhanCuoi; // Nội dung tin nhắn cuối để hiển thị nhanh

    @Column(name = "thoi_gian_tin_nhan_cuoi")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant thoiGianTinNhanCuoi;

    @OneToMany(mappedBy = "phongChat", cascade = CascadeType.ALL, orphanRemoval = true)
    @JsonManagedReference
    @Builder.Default
    private List<ThanhVienPhongChat> thanhVien = new ArrayList<>();

    @Column(name = "da_xoa")
    @Builder.Default
    private Boolean daXoa = false;

    @PrePersist
    protected void onCreate() {
        taoLuc = Instant.now();
        capNhatLuc = Instant.now();
    }

    @PreUpdate
    protected void onUpdate() {
        capNhatLuc = Instant.now();
    }
}
