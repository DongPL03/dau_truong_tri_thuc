package com.app.backend.responses.trandau;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.Instant;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SyncStateResponse {
    @JsonProperty("tran_dau_id")
    private Long tranDauId;

    @JsonProperty("trang_thai")
    private String trangThai; // PENDING/ONGOING/FINISHED

    @JsonProperty("current_question_index")
    private int currentQuestionIndex;

    @JsonProperty("current_question_start")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant currentQuestionStart;

    @JsonProperty("seconds_per_question")
    private int secondsPerQuestion;

    @JsonProperty("current_question_id")
    private Long currentQuestionId;

    @JsonProperty("noi_dung")
    private String noiDung;

    @JsonProperty("loai_noi_dung")
    private String loaiNoiDung;

    @JsonProperty("duong_dan_tep")
    private String duongDanTep;

    @JsonProperty("a")
    private String a;

    @JsonProperty("b")
    private String b;

    @JsonProperty("c")
    private String c;

    @JsonProperty("d")
    private String d;

    @JsonProperty("my_total_points")
    private int myTotalPoints;
}

