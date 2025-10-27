package com.app.backend.models;

import com.app.backend.models.constant.TrangThaiKetBan;
import com.fasterxml.jackson.annotation.JsonBackReference;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Table(name = "ket_ban", uniqueConstraints = {
        @UniqueConstraint(columnNames = {"nguoi_gui_id", "nguoi_nhan_id"})
})
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class KetBan {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "nguoi_gui_id", nullable = false)
    private NguoiDung nguoiGui;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "nguoi_nhan_id", nullable = false)
    private NguoiDung nguoiNhan;

    @Column(name = "trang_thai", columnDefinition = "ENUM('PENDING','ACCEPTED','DECLINED') DEFAULT 'PENDING'")
    private String trangThai = TrangThaiKetBan.PENDING;

    @Column(name = "tao_luc", updatable = false)
    private LocalDateTime taoLuc;

    @PrePersist
    protected void onCreate() {
        taoLuc = LocalDateTime.now();
    }
}
