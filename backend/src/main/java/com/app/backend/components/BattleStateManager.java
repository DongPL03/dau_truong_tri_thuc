package com.app.backend.components;


import com.app.backend.models.BattleState;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

@Component
public class BattleStateManager {
    private final Map<Long, BattleState> activeBattles = new ConcurrentHashMap<>();

    public void save(BattleState state) {
        activeBattles.put(state.getTranDauId(), state);
    }

    public BattleState get(Long tranDauId) {
        return activeBattles.get(tranDauId);
    }

    public void remove(Long tranDauId) {
        activeBattles.remove(tranDauId);
    }

    public boolean exists(Long tranDauId) {
        return activeBattles.containsKey(tranDauId);
    }

    public void cleanupFinishedBattles() {
        activeBattles.entrySet().removeIf(e -> e.getValue().isFinished());
    }

    // ✅ Thêm: xem danh sách trận đang chạy (debug)
    public Set<Long> listActive() {
        return activeBattles.keySet();
    }
}
