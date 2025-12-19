package com.app.backend.services.thongke;

import com.app.backend.responses.thongke.TopicStatResponse;

import java.util.List;

public interface IThongKeChuDeService {

    List<TopicStatResponse> getTopicStatsForUser(Long userId);
}

