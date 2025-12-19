package com.app.backend.models;

import com.app.backend.models.constant.AchievementCode;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(
        name = "nguoi_dung_thanh_tich",
        uniqueConstraints = @UniqueConstraint(columnNames = {"nguoi_dung_id", "code"}),
        indexes = {
                // Khai báo Index để Hibernate tự tạo nếu dùng ddl-auto=update
                @Index(name = "idx_user_unlock", columnList = "nguoi_dung_id, mo_khoa_luc DESC"),
                @Index(name = "idx_code", columnList = "code")
        }
)
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class NguoiDungThanhTich {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "nguoi_dung_id", nullable = false)
    @JsonIgnore // 1. Tránh lỗi vòng lặp vô tận hoặc load dữ liệu thừa khi trả về API
    @ToString.Exclude // Ngăn Lombok gọi toString gây load dữ liệu lazy
    private NguoiDung nguoiDung;

    @Enumerated(EnumType.STRING)
    @Column(name = "code", length = 50, nullable = false)
    private AchievementCode code;

    @Column(name = "mo_ta")
    private String moTa;

    @Column(name = "mo_khoa_luc", nullable = false, updatable = false)
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant moKhoaLuc;
}