package com.app.backend.models;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

/**
 * Entity lưu trữ thông tin chặn người dùng (Block)
 */
@Entity
@Table(name = "chan_nguoi_dung", uniqueConstraints = {
        @UniqueConstraint(columnNames = {"nguoi_chan_id", "nguoi_bi_chan_id"})
})
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ChanNguoiDung {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "nguoi_chan_id", nullable = false)
    private NguoiDung nguoiChan;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "nguoi_bi_chan_id", nullable = false)
    private NguoiDung nguoiBiChan;

    @Column(name = "ly_do")
    private String lyDo;

    @Column(name = "chan_luc", updatable = false)
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant chanLuc;

    @PrePersist
    protected void onCreate() {
        chanLuc = Instant.now();
    }
}
